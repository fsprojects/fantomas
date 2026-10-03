match this.InboundSubnetName with
| None -> ()
| Some subnet -> // Build an inbound for the specified subnet.
    {
        Name = subnet
        Location = location
        DnsResolverId = Managed(dnsResolvers.resourceId this.Name)
        SubnetId =
            Unmanaged {
                vnetId.ResourceId with
                    Type = Arm.Network.subnets
                    Segments = [ subnet ]
            }
        PrivateIpAllocations = [ DynamicPrivateIp ]
        Dependencies = Set.empty
        Tags = Map.empty
    }
