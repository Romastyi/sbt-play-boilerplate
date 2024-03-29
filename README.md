[![Sonatype Nexus (Snapshots)](https://img.shields.io/nexus/s/com.github.romastyi/sbt-play-boilerplate-lib_2.12?server=https%3A%2F%2Foss.sonatype.org)](https://oss.sonatype.org/content/repositories/snapshots/com/github/romastyi/sbt-play-boilerplate-plugin_2.12_1.0/)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.romastyi/sbt-play-boilerplate-plugin/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.romastyi/sbt-play-boilerplate-plugin)

# SBT plugin for Scala code generation for Play! Framework using Swagger 2.0 specs 

## N.B.

This project is based on [sbt-swagger-codegen](https://github.com/unicredit/sbt-swagger-codegen) by UniCredit S.p.A., but provides more complete [Swagger 2.0 specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md) support.

## Overview

Like the official [swagger-codegen](https://github.com/swagger-api/swagger-codegen) this project aims to generate Scala source code from [Swagger 2.0 specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md) compliant API descriptions.
Moreover, you can do it directly within an `sbt` project.

## Compatibility

This code generator is designed specifically for [Swagger 2.0 specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md). Moreover, it relies on [Play! Framework](http://www.playframework.com) for Json marshalling/unmarshalling, server- and client-side code.

| Plugin version | Play 2.3.x | Play 2.4.x | Play 2.5.x | Play 2.6.x | Play 2.7.x |
|:--------------:|:----------:|:----------:|:----------:|:----------:|:----------:|
| 0.1.x          | ✔          | ✔          | ✔          | ✔          | ✘          |  
| 0.2.x          | ✘          | ✔          | ✔          | ✔          | ✔ *        |

*) Experimental support.

## Install

Enable it inside your `project\plugins.sbt` like this:

`addSbtPlugin("com.github.romastyi" % "sbt-play-boilerplate" % "latest version in badge")`

Enable it in your `build.sbt` like this:

`enablePlugins(PlayBoilerplatePlugin)`

## Quick start

For a *super fast* hands-on tutorial refer to the related examples and check out [sbt-play-boilerplate-examples](https://github.com/romastyi/sbt-play-boilerplate-examples).

## Authors:

**Original [sbt-swagger-codegen](https://github.com/unicredit/sbt-swagger-codegen) project authors:**
* Andrea Peruffo: <https://github.com/andreaTP>
* Francesco Montecuccoli Degli Erri <https://github.com/fralken>
* Marco Firrincieli: <https://github.com/mfirry>

**This project:**
* Puchka Roman: <https://github.com/romastyi>
