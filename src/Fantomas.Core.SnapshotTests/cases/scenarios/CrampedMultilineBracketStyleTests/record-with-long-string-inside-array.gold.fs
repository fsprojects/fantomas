type Database =

    static member Default() =
        Database.Lowdb
            .defaults(
                { Version = CurrentVersion
                  Questions =
                    [| { Id = 0
                         AuthorId = 1
                         Title = "What is the average wing speed of an unladen swallow?"
                         Description =
                           """
Hello, yesterday I saw a flight of swallows and was wondering what their **average wing speed** is?

If you know the answer please share it.
                             """
                         Answers =
                           [| { Id = 0
                                CreatedAt = DateTime.Parse "2017-09-14T19:57:33.103Z"
                                AuthorId = 0
                                Score = 2
                                Content =
                                  """
> What do you mean, an African or European Swallow?
>
> Monty Python’s: The Holy Grail

Ok I must admit, I use google to search the question and found a post explaining the reference :).

I thought you were asking it seriously, well done.
                                    x""" }
                              { Id = 1
                                CreatedAt = DateTime.Parse "2017-09-14T20:07:27.103Z"
                                AuthorId = 2
                                Score = 1
                                Content =
                                  """
Maxime,

I believe you found [this blog post](http://www.saratoga.com/how-should-i-know/2013/07/what-is-the-average-air-speed-velocity-of-a-laden-swallow/).

And so Robin, the conclusion of the post is:

> In the end, it’s concluded that the airspeed velocity of a (European) unladen swallow is about 24 miles per hour or 11 meters per second.
                                    """ } |]
                         CreatedAt = DateTime.Parse "2017-09-14T17:44:28.103Z" }
                       { Id = 1
                         AuthorId = 0
                         Title = "Why did you create Fable?"
                         Description =
                           """
Hello Alfonso,

I wanted to know why you created Fable. Did you always plan to use F#? Or were you thinking in others languages?
                             """
                         Answers = [||]
                         CreatedAt = DateTime.Parse "2017-09-12T09:27:28.103Z" } |]
                  Users =
                    [| { Id = 0
                         Firstname = "Maxime"
                         Surname = "Mangel"
                         Avatar = "maxime_mangel.png" }
                       { Id = 1
                         Firstname = "Robin"
                         Surname = "Munn"
                         Avatar = "robin_munn.png" }
                       { Id = 2
                         Firstname = "Alfonso"
                         Surname = "Garciacaro"
                         Avatar = "alfonso_garciacaro.png" }
                       { Id = 3
                         Firstname = "Guest"
                         Surname = ""
                         Avatar = "guest.png" } |] }
            )
            .write()

        Logger.debug "Database restored"
