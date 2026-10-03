let x =
    JobCollectionCreateParameters(
        Label = "Test",
        IntrinsicSettings =
            JobCollectionIntrinsicSettings(
                Plan = JobCollectionPlan.Standard,
                Quota = new JobCollectionQuota(MaxJobCount = Nullable(50))
            )
    )
