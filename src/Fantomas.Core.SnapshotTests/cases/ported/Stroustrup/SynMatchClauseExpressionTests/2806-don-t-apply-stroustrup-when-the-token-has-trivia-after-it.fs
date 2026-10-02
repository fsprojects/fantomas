(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
match this.InboundSubnetName with
| None -> ()
| Some subnet -> // Build an inbound for the specified subnet.
    {
        Name = subnet
        Location = location
        DnsResolverId = Managed(dnsResolvers.resourceId this.Name)
        SubnetId =
            Unmanaged
                { vnetId.ResourceId with
                    Type = Arm.Network.subnets
                    Segments = [ subnet ]
                }
        PrivateIpAllocations = [ DynamicPrivateIp ]
        Dependencies = Set.empty
        Tags = Map.empty
    }
