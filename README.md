# bartlett

A simple Jenkins command line client to serve your needs.

#### Motivation

We live on the command line, and anything that can help us stay there longer is
a boon to productivity. While the Jenkins web interface is nice for many, it is
a distracting context switch for us.

My goal for this tool is to replicate many of the workflows that we use
day-to-day through the web interface in a single, easy to use command line
client. Additionally, many of the existing clients are either not under active
development or do not follow the same usage patterns as Bartlett.

##### Why not just use the Jenkins CLI jar?

A few reasons:

  1. `bartlett`'s focus is on translating workflows from the web ui to the
  command line.
      * It is _not_ mean to be a replacement for the Jenkins CLI jar, where the
      primary focus is on remotely administrating a Jenkins instance.
  2. `bartlett`'s output is primarly JSON, which means that it can be piped
  into tools like [jq][jq-page] and scripted programmatically.
  3. Profile support to alleviate the tedium of working with multiple Jenkins
  instances
      * Similar in spirit to AWS CLI profiles
  4. Some Jenkins instances are not configured to allow JNLP access
      * `bartlett` talks to Jenkins over its REST API and does not suffer this
      limitation.
  5. We want a tool that could be installed as a static binary

##### And why not just use curl?

You could, but you'll end up tying a lot more in the long run. `bartlett`'s
support for profiles means that authentication and Jenkins instance resolution
are done for you at invocation. You also don't have to worry about exposing
your password since `bartlett` doesn't accept it as a configuration or
command line option (only requested at runtime with hidden input).

## Installation

### from Homebrew

Make sure you have [Homebrew installed][homebrew-install] before proceeding.

<span style="color:red;">TODO: add Homebrew recipe upstream for bartlett</span>

```bash
brew install bartlett
```

#### Updating

Homebrew updates itself by pulling down the latest revisions of the git
repositories that provide its recipes. To upgrade to the latest version of
Bartlett begin by updating Homebrew itself before upgrading Bartlett.

```bash
brew update && brew ugprade bartlett
```

### from Source

Make sure you have [Stack][stack-install] installed before you begin.

Change directory to where you store your development projects:

```
git clone https://github.com/Nike-Inc/bartlett.git
cd bartlett && stack build && stack install
```

## Getting Help

Please file an issue in the issue tracker with as complete a description of your
issue as possible.

## Usage

##### A note about protocols

Bartlett will honor any protocol explicitly passed on the command line or via
configuration. However, if no protocol is provided then Bartlett will attempt
to contact your Jenkins instance via HTTPS. It is _strongly_ recommended that
you talk to your Jenkins instance via HTTPS if possible.

### Getting Help at the Command Line

You can get a list of available options with the `-h` flag:

```
$ bartlett -h
bartlett - the Jenkins command-line tool to serve your needs.

Usage: bartlett [-u|--username USERNAME] [-j|--jenkins JENKINS_INSTANCE]
                [-p|--profile PROFILE_NAME] COMMAND

Available options:
  -h,--help                Show this help text
  -u,--username USERNAME   The user to authenticate with
  -j,--jenkins JENKINS_INSTANCE
                           The Jenkins instance to interact with
  -p,--profile PROFILE_NAME
                           The profile to source values from

Available commands:
  info                     Get information on the given job
  build                    Trigger a build for the given job

Copyright (c) Nike, Inc. 2016
```

### Querying Existing Jobs

You can query for basic information about a given job by providing the path
from root of your Jenkins instance to the desired job.

For example, if my job exists at
`https://my.jenkins-instance.com/job/TEST/job/testJob/`, then I can query
this job's information like so:

```
bartlett --username my_user --jenkins https://my.jenkins-instance.com info TEST/testJob
```

You can also pass this output directly to the [jq][jq-page] tool to query data
even further:

```
$ bartlett --username my_user \
    --jenkins https://my.jenkins-instance.com info TEST/testJob \
    | jq '.jobs | .[] | .name'
"00_my-first-job"
"01_my-second-job"
"02_my-third-job"
```

You can even pass in multiple jobs at once by separating each job path with
a space:

```
$ bartlett -u my_user -j https://my.jenkins-instance.com \
  info FOO BAR | jq '.jobs | .[] | .name'
Enter password:
"foojob-one"
"foojob-two"
"barjob-one"
"barjob-two"
```

### Triggering Job Builds

You can build parameterized and normal jobs by using the `build` sub-command.

For example, if my job exists at
`https://my.jenkins-instance.com/job/~my_user/job/test`, then I can trigger its
build like so:

```
$ bartlett --username my_user \
    --jenkins https://my.jenkins-instance.com build /~my_user/test
Enter password:
{
    "status": "201"
}
```

Or, if I have a job with parameters, I can pass these parameters in using the
`-p` flag.

```
$ bartlett --username my_user --jenkins https://my.jenkins-instance.com \
    build /~my_user/test --options FOO=bar,BAZ=quux
Enter password:
{
    "status": "201"
}
```

### Configuring Profiles

You may store configuration values for many different Jenkins instances. First
create a bartlett configuration file:

```bash
touch ~/.bartlett.cfg && $EDITOR ~/.bartlett.cfg
```

By default, values will attempt to be sourced from the `default` configuration
block.

```
# The default profile
default {
  # Where should Bartlett route its requests?
  jenkins_instance = "https://my.jenkins-instance.com"
  # What username should be used to authenticate requests?
  username = "my_user"
}

# Additional profile
dank_profile {
  jenkins_instance = "https://dank.jenkins-instance.com"
  username = "wewlad"
}
```

You can then invoke Bartlett without providing user or Jenkins options:

```bash
bartlett info /  # Uses the default profile from above
bartlett --profile dank_profile info /  # Source a different profile
```

If a value is provided on the command line _AND_ configured in a profile, then
the value provided on the command line will take precedence.

## Development

Make sure you have [Stack][stack-install] installed before you begin.

Then build the project:

```bash
stack build
```

Or alternatively start a REPL to test things out interactively:

```bash
stack ghci
```

#### Running Tests on File Change

When actively working on a feature I'll typically run the following to get
automatic feedback as I write code:

```bash
stack build --test --coverage --haddock --copy-bins --file-watch
```

Or run the make target:

```
make watch
```

To exit out of this loop type `quit` (instead of C-c).

#### Building a Static Binary

Surprise, more Stack options!

```bash
stack build --ghc-options='-optl-static -optl-pthread' --force-dirty --haddock --copy-bins
```

Or use the make target:

```bash
make package-bin
```

### Deployment

It's too manual, but the gist is:

  1. Run `make package-bin`
  2. Rename the tarball to `bartlett-static-{version}.tar.gz`
  3. Upload it to the tools account S3 bucket under `bartlett-dist`
  4. Grab the URL from the properties for that tarball and then update the
  associated recipe in the Homebrew tap

### What's in a name?

[Leslie Bartlett][bartlett-wiki] was a famous butler who founded the London
School of British Butlers.

[bartlett-wiki]: https://en.wikipedia.org/wiki/Leslie_Bartlett
[stack-install]: https://docs.haskellstack.org/en/stable/README/
[jq-page]: https://stedolan.github.io/jq/
[homebrew-install]: http://brew.sh/
