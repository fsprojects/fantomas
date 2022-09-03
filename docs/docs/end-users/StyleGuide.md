---
category: End-users
categoryindex: 1
index: 2
---
# Style guide

Fantomas tries to adhere to two F# style guides:
- [Microsoft](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting)
- [G-Research](https://github.com/G-Research/fsharp-formatting-conventions)

By default Fantomas  will format the code according to the Microsoft guide.   
The benefit of these guides is that this allows us, as the F# community, to write code in the same manner.  

## Let it go

If you are not used to having a code formatter, you might struggle a bit at first. A part of using a code formatter is about letting go how you wrote things and accept a common consistent style instead.  
When embraced, this can truly be **a liberation feeling**. You can play _jazz_ while typing the code, focus on the task at hand and have the same output as if anyone in your team would have written it.

## A cautionary tale

The responsibility of the coding style is also a shared one. The maintainers of Fantomas do not dictate how things should look like.  
Instead, conversations are held over at [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide).  

Over the years, a lot of consumers of Fantomas have requested numerous features. These often go against the philosophy of being consistent amongst the community.
If there were a _gazillion of settings_ to please everybody it would be a _missed opportunity_ to see all F# code bases in the same style.  

And more importantly the maintenance cost of each setting is always carried by only a handful of people. **The hardship of having a setting is so high that in almost all cases it is not worth having it**.  
Even when someone proposes to contribute the setting, it always leads to lament and sorrow for the maintainers to carry. 

## Default style guide

As mentioned, the default style guide is not to be discussed on the Fantomas repository. The majority of the maintainers is burnt out talking about style at this point.  
If you want to discuss the style, you are more than welcome on [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide).  

However, we greatly encourage that you take some time to **understand the Fantomas code base**.  Having an opinion on code style is easy, everybody writes code so we all have our thoughts and feelings about it.
Being able to **translate your thoughts/opinions** and **prototyping them in Fantomas** will gain you a **much larger insight** on how quickly things can get quite complex.  

At the end of the day, if some new idea is validated on `fsharp/fslang-design`, it needs to be:

- Documented in the style guide (over at [dotnet/docs](https://github.com/dotnet/docs/blob/main/docs/fsharp/style-guide/formatting.md))
- Implemented in `Fantomas`

> There’s no such thing as a free lunch, so please consider all aspects of debating code style.

## G-Research Style

[G-Research](https://www.gresearch.co.uk/) has been our _single enterprise sponsor_ for the last five years. **Fantomas would not exist** in it's current form if it were **not for their support**.  
Their interest was to have a tool that automatically format code according to their _internal guidelines_. These have been made available publicly and can be turned on via [various settings](https://github.com/G-Research/fsharp-formatting-conventions/blob/master/.editorconfig).

## Recommendations

To strengthen the message of unity we advice that you do not change the default settings.
The **out-of-the-box experience** should be a result of what the **brightest minds of the community** came up with.
If you are _new_ to the F# language, this is what you want.

<fantomas-nav previous="./GettingStarted.html" next="./Configuration.html"></fantomas-nav>
