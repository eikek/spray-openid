# spray openid

This is a [OpenId](http://openid.net/) (v2) library providing consumer
and producer support for use with [spray](http://spray.io) (1.2.0).

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

* [spray-client](http://spray.io/documentation/1.2.0/spray-client/) and
* [spray-routing](http://spray.io/documentation/1.2.0/spray-routing/)

and needs therefore transitive dependencies of both (i.e. the
parboiled library for doing base64 de/encoding).

## Using the Consumer

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
[here](src/test/scala/org/eknet/spray/openid/RelyingParty.scala).


## Using the Provider

Creating a provider is a bit more involved, since you need to fill the
missing parts. An OpenId provider works by authenticating an end user
and redirecting its user agent back to the relying party site with an
assertion about its identifier. Thus it needs to render a login page
and also an additionaly confirmation page. Page rendering is a bit beyond
the scope of this library, but a simple server side page rendering functionality
is provided to get started quickly.

The OpenId endpoint, provided by `EndpointRoute` class needs a `ProviderHooks`
impl which collects all missing parts in one trait. The provided one defines
defaults for most parts and only expects a spray `Directive` that authenticates
the current request. Furthermore an actor is needed to keep track of associations.

For the `DiscoveryRoute` the `DiscoverySetting` specifies for when to respond
to discovery requests. Both are combined in `ProviderRoute`.

A complete example is
[here](src/test/scala/org/eknet/spray/openid/Provider.scala).

## Todo

This is a work in progress. The library supports authentication and
the
[SReg extension](http://openid.net/specs/openid-simple-registration-extension-1_0.html);
but there is still a lot to do, for example:

* write some tests and then fix the bugs
* make a serious attempt to implement the discovery stuff
* make it work with xri identifiers (since I have no clue what that
  is, you'll receive an exception when trying)
* compatibility with OpenId v1 (?)

Despite these things, the consumer part works with the providers I tried so far
(i.e. Google and MyOpenId) and I found the provider part working with `OpenId4Java`.