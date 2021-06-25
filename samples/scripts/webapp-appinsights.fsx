#r "../../src/Farmer/bin/Debug/netstandard2.0/Farmer.dll"

open Farmer
open Farmer.Builders
open Farmer.Arm
open Farmer.Arm.ResourceGroup

let template =
    let domainName = "devops-test.codat.io"
    let myWebApp = webApp {
        name "codat-devopstest"
        sku WebApp.Sku.B1
        custom_domain (CertConfig.AppServiceCertificate domainName)
        app_insights_off
        runtime_stack Runtime.DotNetCore31
    }

    //let hostNameBinding : Arm.Web.HostNameBinding = {
    //    Location = Location.UKSouth
    //    SiteId =  Managed (Arm.Web.sites.resourceId myWebApp.Name)
    //    DomainName = domainName
    //    SslState = SslDisabled //SslState.Sni (ArmExpression.reference(Arm.Web.certificates, Arm.Web.certificates.resourceId cert.ResourceName).Map(sprintf "%s.Thumbprint"))
    //}

    //let cert : Arm.Web.Certificate =  {
    //    Location = Location.UKSouth
    //    SiteId = myWebApp.ResourceId
    //    ServicePlanId = myWebApp.ServicePlanId
    //    DomainName = domainName
    //}

    //let nested = resourceGroup{
    //    name "my-resource-group-name-2"
    //    location Location.UKSouth
    //    depends_on [ hostNameBinding.ResourceId ]
    //    //depends_on [ Arm.Web.sites.resourceId myWebApp.Name]
    //    depends_on [ Arm.Web.certificates.resourceId cert.ResourceName]
    //    add_resource {
    //        hostNameBinding with
    //            SslState = SslState.Sni (ArmExpression.reference(Arm.Web.certificates, Arm.Web.certificates.resourceId cert.ResourceName).Map(sprintf "%s.Thumbprint"))
    //            SiteId = 
    //                match hostNameBinding.SiteId with 
    //                | Managed id -> Unmanaged id
    //                | x -> x
    //    }
    //}



    //let hostNameBinding = { hostNameBinding with 
    //                        SslState = SslState.Sni (ArmExpression.reference(Arm.Web.certificates, Arm.Web.certificates.resourceId cert.ResourceName).Map(sprintf "%s.Thumbprint")) }

    arm {
        location Location.UKSouth
        add_resource myWebApp
        //add_resource hostNameBinding
        //add_resource cert
        //add_resource nested
    }

template
//|> Writer.quickWrite "army"
|> Deploy.execute "my-resource-group-name-2" Deploy.NoParameters
