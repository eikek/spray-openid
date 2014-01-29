# spray openid

This is a [OpenId](http://openid.net/) (v2) client or consumer library
for use with [spray](http://spray.io) (1.2.0).

Since spray's focus is not really web applications, there is probably
little need for it. But using spray for web applications is actually
quite nice, so in some cases openid authentication might be useful and
so maybe this library.

## Building

Scala 2.10 is used and [sbt](http://scala-sbt.org) to build the
sources:

    $ sbt package

The jar file is then in `target/scala-2.10`.

This project depends on
[spray-client](http://spray.io/documentation/1.2.0/spray-client/) and
[spray-routing](http://spray.io/documentation/1.2.0/spray-routing/)
and needs therefore transitive dependencies of both (i.e. the
parboiled library for doing base64 de/encoding).

## Usage

One actor is needed for storing associations and tracking response
nonces, which are used to verify authentication responses. You need to
create the actor somewhere and pass the `ActorRef` to the directive.

The directive will execute its given route on successful
authentication and pass the response to it:

```scala
verifyId(OpenIdAuth(actorRef)) { id =>
  complete(s"Login Successful: ${id.claimedId}")
} ~
complete("Login failed.")
```

The directive either verifies an incoming assertion request from the
OpenId provider or uses the `suppliedId` to initiate the OpenId
authentication request. The `suppliedId` is either read from a form
field or parameter with name `openid_identifier` or directly given to
the `OpenIdAuth` object.

A complete example is
[here](tree/master/src/test/scala/org/eknet/spray/openid/TestApp.scala).

## Todo

This is a work in progress. The library supports authentication and
the
[simple registration extension](http://openid.net/specs/openid-simple-registration-extension-1_0.html);
but there is still a lot to do, for example:

* write some tests and then fix the bugs
* make a serious attempt to implement the discovery stuff
* make it work with xri identifiers (since I have no clue what that
  is, you'll receive an exception when trying)
* compatibility with OpenId v1 (?)

Despite these things, it works with the providers I tried so far
(i.e. Google and MyOpenId).
