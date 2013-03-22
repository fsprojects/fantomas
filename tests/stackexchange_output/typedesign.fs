/// The program is from http://codereview.stackexchange.com/q/20852
module Typedesign

type Metrics = 
  | Revenue
  | Volume
  | PriceAsp
  member m.FormatGroupings = 
    match m with
    | Revenue -> [("revenue", """$sum: { "$Amount" }" """)]
    | Volume -> [("volume", """$sum: { "$Quantity" }" """)]
    | PriceAsp -> List.append Revenue.FormatGroupings Volume.FormatGroupings
  member m.FormatProjections = 
    match m with
    | Revenue -> [("revenue", "$revenue")]
    | Volume -> [("volume", "$volume")]
    | PriceAsp -> [("priceAsp", """ $divide: ["$revenue", "$volume"]  """)]

let buildQuery groupBy (metrics : Metrics list) = 
  let concatenate f = 
    let x = metrics
                    |> List.collect f
            |> List.map(fun (m) -> sprintf "{%s: %s}" (fst m) (snd m))
    System.String.Join(",", x)
  let groupings = concatenate(fun (m) -> m.FormatGroupings)
  let projections = concatenate(fun (m) -> m.FormatProjections)
  sprintf """{$group: {_id: {%s: "$%s"}}, %s}, $project: {%s}}""" groupBy groupBy groupings projections
