# elm-procedure-architecture

[![Build Status](https://app.travis-ci.com/arowM/elm-procedure-architecture.svg?branch=main)](https://app.travis-ci.com/arowM/elm-procedure-architecture)  
[Document](https://package.elm-lang.org/packages/arowM/elm-procedure-architecture/latest/)  
[Live demo 1](https://arowm.github.io/elm-procedure-architecture/index.html)  
[Live demo 2](https://arowm.github.io/elm-procedure-architecture/list-items.html)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

Extend TEA so that complex processing procedures can be written as they are.

# What is this for?

With elm-procedure-architecture, you can translate verbatim the specification of a UX-aware application into an implementation with the same look and feel.

In a UX-aware application, it is natural to write the specification in chronological order.
This is because application users make decisions about what to do next, based on their experience of their previous operations and the application's response to those operations.
However, conventional TEA is not suitable for implementing such specifications: Every time the user interacts with the screen, you have to check the model in the `update` function and try hard to analyze "what time series did the user follow" to choose the next process. A lot of bugs are introduced in this kind of transformation work. The bad news is that these bugs are about the behaviour of the application, so you have to suffer through complex and difficult UI testing.

With elm-procedure-architecture, you can solve such drawbacks of TEA. As shown in the following example, it is possible to implement time series processing as it looks. What a magical library!

# Terms

The terms referred to in this document are defined as follows:

* Procedure: Definitions of the processes that the application will perform, in order.
* Memory: State of the application, just like the Model in TEA.
* Event: Events triggered by the user or the external environment, just like the Message in TEA.
    * TEA Messages are _global_: there is no concept of destination.
    * The elm-procedure-architecture Events can be _local_: there is concept of destination _Observer_.
        * This frees you from the problem of concurrent processes interfering with each other.
* Observer: Focuses on a specific part of Memory and monitors events.

# A Quick Example

TODO
