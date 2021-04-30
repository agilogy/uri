## Installation

```
resolvers += "Agilogy GitLab" at "https://gitlab.com/api/v4/groups/583742/-/packages/maven"

libraryDependencies += "com.agilogy" %% "uris" % "0.2"
```

## Publishing

To publish this package to Agilogy's Package Registry, set the `GITLAB_DEPLOY_TOKEN` environment variable and then run the following command in sbt:

```
sbt:play-json-hierarchy> +publish
```
