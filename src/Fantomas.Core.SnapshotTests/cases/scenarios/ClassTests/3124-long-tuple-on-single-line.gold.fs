type Y =
    static member putItem
        (client: AmazonDynamoDBClient, tableName: string, attributeValueDict: Dictionary<string, AttributeValue>)
        : TaskResult<unit, Error> =
        ()
