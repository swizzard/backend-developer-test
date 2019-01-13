Notes, Changelog, Etc.

General Notes
---
My approach to this project is 'inside-out', in that I'm going to try to create pure functions that create Users, Games, etc., then write functions to interact with the database, then write the web API, then write the frontend, then deal with deployment.


* I'm using Docker because it's similar to Kubernetes, but I'm more familiar with it and would rather maximize Haskell time and minimize DevOps time.
* I'm using [PostgREST](http://postgrest.org/en/v5.2/) because
    * Microservices!
    * ORMs can be headache-inducing, generally:
        * One source of truth > multiple sources of truth.
        * Treating everything as pure JSON input/output makes development easier:
            * Putting some credentials and a URL in the `r` of one's `Reader r a` is
              much less of a hassle than dealing with e.g. connection pools.
            * I don't have to worry about mocks, fixtures, etc. come testing time.
            * It's easier to put off actually dealing with the DB until later in the process, which means more settled types/API, which means fewer migrations.
    * It extremely rules.
* I'm planning on using [Servant](https://haskell-servant.readthedocs.io/en/stable/) because
    * I don't have a strong attachment to any particular Haskell web framework.
    * Y'all said "API" so I thought Servant.
    * I'm not planning on dealing with static files or complicated session management stuff or any of the other more traditional webdev headaches that would cause one to reach for a fuller-service framework like Yesod or Scotty.
    * PostgREST already speaks JSON, and I like the way Servant does automatic JSON de/serialization, with the option for other content types in case I want them.
* I'm planning on using [Netlify](https://netlify.com) for the frontend because
    * I've used it before (for my [personal site](https://swizzard.pizza) and the [site I made for my brother](https://henryraker.com)) and it's really easy to deal with.
    * I'm not planning on doing any server-side template rendering so a "static" frontend works well enough
    * [Authenticating through Google](https://developers.google.com/identity/sign-in/web/) is primarily an issue of dropping in some JavaScript and calling it a day. I'm going to try to minimize the amount of JavaScript I write for this project, mostly because the I'm already doing enough Dockering to want to avoid having to WebPack or whatever.

Day 1
---
Docker. Hoo boy. Everything seems to work, and I can edit files locally and see the changes in the container, at least. I realize now this is kind of in violation of my "inside-out" approach, but I'd honestly much rather deal with it now while I have time than after I've got everything "working" but in a way that can't be deployed or interacted with outside of my laptop.

Day 2
---
Seriously start the Haskell portion.
* Types! I always start with types because getting to (rigorously-ish) model the domain is one of the parts of Haskell I appreciate the most--it encourages you to really think about what you're trying to do and how.
    * Because I'm using PostgREST to interact with the DB, [Aeson](http://hackage.haskell.org/package/aeson) is largely stepping into the role that would be filled by any ORM or Esqueleto/BEAM/etc--it's easy to map my types to/from JSON, with enough template Haskell to feel like an adult.
    * Speaking of big-person Haskell, I've just gone ahead and [Lens](http://hackage.haskell.org/package/lens-4.17)'d up my types from the start, because it's an investment that continues to pay dividends. I'm not doing anything too serious with it yet (mostly just `^.`/`views`) but it'll become increasingly useful when I start really building out the CRUDdier parts of the application.
    * I also had an excuse to dig into Arrows to solve a problem that irks me fairly often: doing multiple things to/getting multiple bits of a single argument and combining the results. See [Misc.hs](haskell/src/Misc.hs) and [Game.hs](haskell/src/Types/Game.hs) for more.
