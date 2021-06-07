module Functions

open Expecto
open Farmer
open Farmer.Builders
open Farmer.Arm
open Microsoft.Azure.Management.WebSites
open Microsoft.Azure.Management.WebSites.Models
open Microsoft.Rest
open System
open Farmer.Identity
open Farmer.WebApp

let getResource<'T when 'T :> IArmResource> (data:IArmResource list) = data |> List.choose(function :? 'T as x -> Some x | _ -> None)
/// Client instance needed to get the serializer settings.
let dummyClient = new WebSiteManagementClient (Uri "http://management.azure.com", TokenCredentials "NotNullOrWhiteSpace")
let getResourceAtIndex o = o |> getResourceAtIndex dummyClient.SerializationSettings
let getResources (b:#IBuilder) = b.BuildResources Location.WestEurope

let tests = testList "Functions tests" [
    test "Renames storage account correctly" {
        let f = functions { name "test"; storage_account_name "foo" }
        let resources = (f :> IBuilder).BuildResources Location.WestEurope
        let site = resources.[0] :?> Web.Site
        let storage = resources.[2] :?> Storage.StorageAccount

        Expect.contains site.Dependencies (storageAccounts.resourceId "foo") "Storage account has not been added a dependency"
        Expect.equal f.StorageAccountName.ResourceName.Value "foo" "Incorrect storage account  name on site"
        Expect.equal storage.Name.ResourceName.Value "foo" "Incorrect storage account name"
    }
    test "Implicitly sets dependency on connection string" {
        let db = sqlDb { name "mySql" }
        let sql = sqlServer { name "test2"; admin_username "isaac"; add_databases [ db ] }
        let f = functions { name "test"; storage_account_name "foo"; setting "db" (sql.ConnectionString db) } :> IBuilder
        let site = f.BuildResources Location.NorthEurope |> List.head :?> Web.Site
        Expect.contains site.Dependencies (ResourceId.create (Sql.databases, ResourceName "test2", ResourceName "mySql")) "Missing dependency"
    }
    test "Works with unmanaged storage account" {
        let externalStorageAccount = ResourceId.create(storageAccounts, ResourceName "foo", "group")
        let functionsBuilder = functions { name "test"; link_to_unmanaged_storage_account externalStorageAccount }
        let f = functionsBuilder :> IBuilder
        let resources = f.BuildResources Location.WestEurope
        let site = resources |> List.head :?> Web.Site

        Expect.isFalse (resources |> List.exists (fun r -> r.ResourceId.Type = storageAccounts)) "Storage Account should not exist"
        Expect.isFalse (site.Dependencies |> Set.contains externalStorageAccount) "Should not be a dependency"
        Expect.stringContains site.AppSettings.["AzureWebJobsStorage"].Value "foo" "Web Jobs Storage setting should have storage account name"
        Expect.stringContains site.AppSettings.["AzureWebJobsDashboard"].Value "foo" "Web Jobs Dashboard setting should have storage account name"
    }
    test "Handles identity correctly" {
        let f : Site = functions { name "" } |> getResourceAtIndex 0
        Expect.equal f.Identity.Type (Nullable ManagedServiceIdentityType.None) "Incorrect default managed identity"
        Expect.isNull f.Identity.UserAssignedIdentities "Incorrect default managed identity"

        let f : Site = functions { system_identity } |> getResourceAtIndex 0
        Expect.equal f.Identity.Type (Nullable ManagedServiceIdentityType.SystemAssigned) "Should have system identity"
        Expect.isNull f.Identity.UserAssignedIdentities "Should have no user assigned identities"

        let f : Site = functions { system_identity; add_identity (createUserAssignedIdentity "test"); add_identity (createUserAssignedIdentity "test2") } |> getResourceAtIndex 0
        Expect.equal f.Identity.Type (Nullable ManagedServiceIdentityType.SystemAssignedUserAssigned) "Should have system identity"
        Expect.sequenceEqual (f.Identity.UserAssignedIdentities |> Seq.map(fun r -> r.Key)) [ "[resourceId('Microsoft.ManagedIdentity/userAssignedIdentities', 'test2')]"; "[resourceId('Microsoft.ManagedIdentity/userAssignedIdentities', 'test')]" ] "Should have two user assigned identities"

    }

    test "Supports always on" {
        let f:Site = functions { name "" } |> getResourceAtIndex 0
        Expect.equal f.SiteConfig.AlwaysOn (Nullable false) "always on should be false by default"

        let f:Site = functions { always_on } |> getResourceAtIndex 0
        Expect.equal f.SiteConfig.AlwaysOn (Nullable true) "always on should be true"
    }

    test "Supports 32 and 64 bit worker processes" {
        let f:Site = functions { worker_process Bitness.Bits32 } |> getResourceAtIndex 0
        Expect.equal f.SiteConfig.Use32BitWorkerProcess (Nullable true) "Should use 32 bit worker process"

        let f:Site = functions { worker_process Bitness.Bits64 } |> getResourceAtIndex 0
        Expect.equal f.SiteConfig.Use32BitWorkerProcess (Nullable false) "Should not use 32 bit worker process"
    }

    test "FunctionsApp supports adding slots" {
        let slot = appSlot { name "warm-up" }
        let site = functions { add_slot slot }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>

        Expect.hasLength slots 1 "Should only be 1 slot"
    }

    test "Functions App with slot adds managed identity to slot" {
        let slot = appSlot { name "warm-up" }
        let site:FunctionsConfig = functions { 
            add_slot slot
        }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength slots 1 "Should only be 1 slot"

        let expected = { SystemAssigned = Enabled; UserAssigned = [] }
        Expect.equal (slots.Item 0).Identity expected "Slot should have slot setting"
    }

    test "Functions App with slot adds settings to slot" {
        let slot = appSlot { name "warm-up" }
        let site:FunctionsConfig = functions { 
            add_slot slot 
            setting "setting" "some value"
        }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength slots 1 "Should only be 1 slot"

        Expect.isTrue ((slots.Item 0).AppSettings.ContainsKey("setting")) "Slot should have slot setting"
    }

    test "Functions App with slot does not add settings to app service" {
        let slot = appSlot { name "warm-up" }
        let config = functions { 
            add_slot slot 
            setting "setting" "some value"
        }

        let sites = 
            config 
            |> getResources
            |> getResource<Farmer.Arm.Web.Site>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength sites 1 "Should only be 1 slot"
        
        Expect.isFalse ((sites.Item 0).AppSettings.ContainsKey("setting")) "App service should not have any settings"
    }
    
    test "Functions App adds literal settings to slots" {
        let slot = appSlot { name "warm-up" }
        let site:FunctionsConfig = functions { add_slot slot; operating_system Windows }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength slots 1 "Should only be 1 slot"

        let settings = (slots.Item 0).AppSettings
        let expectation = [
            "FUNCTIONS_WORKER_RUNTIME"
            "WEBSITE_NODE_DEFAULT_VERSION"
            "FUNCTIONS_EXTENSION_VERSION"
            "AzureWebJobsStorage"
            "AzureWebJobsDashboard"
            "APPINSIGHTS_INSTRUMENTATIONKEY"
            "WEBSITE_CONTENTAZUREFILECONNECTIONSTRING"
            "WEBSITE_CONTENTSHARE"] |> List.map(settings.ContainsKey)
        Expect.allEqual expectation true "Slot should have all literal settings"
    }

    test "Functions App with different settings on slot and service adds both settings to slot" {
        let slot = appSlot { 
            name "warm-up" 
            setting "slot" "slot value"
        }
        let site:FunctionsConfig = functions { 
            add_slot slot 
            setting "appService" "app service value"
        }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength slots 1 "Should only be 1 slot"
 
        let settings = (slots.Item 0).AppSettings;
        Expect.isTrue (settings.ContainsKey("slot")) "Slot should have slot setting"
        Expect.isTrue (settings.ContainsKey("appService")) "Slot should have app service setting"
    }
    
    test "Functions App with slot, slot settings override app service setting" {
        let slot = appSlot { 
            name "warm-up" 
            setting "override" "overridden"
        }
        let site:FunctionsConfig = functions { 
            add_slot slot 
            setting "override" "some value"
        }
        Expect.isTrue (site.Slots.ContainsKey "warm-up") "Config should contain slot"

        let slots = 
            site 
            |> getResources
            |> getResource<Slot>
        // Default "production" slot is not included as it is created automatically in Azure
        Expect.hasLength slots 1 "Should only be 1 slot"

        let (hasValue, value) = (slots.Item 0).AppSettings.TryGetValue("override");

        Expect.isTrue hasValue "Slot should have app service setting"
        Expect.equal value.Value "overridden" "Slot should have correct app service value"
    }
]