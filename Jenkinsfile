#!groovy

def imageTags = "BASE_IMAGE_TAG=117a2e28e18d41d4c3eb76f5d00af117872af5ac BUILD_IMAGE_TAG=ef20e2ec1cb1528e9214bdeb862b15478950d5cd"

def finalHook = {
  runStage('store CT logs') {
    dir('snakeoil') {
      archive '_build/test/logs/'
    }
  }
}

build('erlang-service-template', 'docker-host', finalHook) {
  runStage('clone build_utils') {
    withGithubSshCredentials {
      sh "git clone git@github.com:rbkmoney/build_utils.git build_utils"
    }
  }

  def pipeDefault
  def withWsCache
  runStage('load library pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    pipeDefault = load("${env.JENKINS_LIB}/pipeDefault.groovy")
    withWsCache = load("${env.JENKINS_LIB}/withWsCache.groovy")
  }

  // erlang-service-template
  ws {
    try {
      checkoutRepo()
      loadBuildUtils()

      runStage('generate erlang service: snakeoil') {
        sh 'make wc_gen_service'
      }

      runStage('archive snakeoil') {
        archive 'snakeoil/'
      }
    } finally {
      runStage('cleanup sub ws') {
        sh 'rm -rf * .* || echo ignore'
      }
    }
  } //close other ws

  runStage('unarchive snakeoil') {
    unarchive mapping: ['snakeoil/': '.']
    sh 'chmod 755 snakeoil/Dockerfile.sh snakeoil/docker-compose.sh'
  }

  dir('snakeoil') {
    runStage('init git repo') {
      sh 'git init'
      sh 'git config user.email "$CHANGE_AUTHOR_EMAIL"'
      sh 'git config user.name "$COMMIT_AUTHOR"'
      sh 'git add README.md'
      sh 'git commit -m "Initial commit"'
    }

    runStage('add git submodule') {
      withGithubSshCredentials {
        sh "git submodule add -b master git@github.com:rbkmoney/build_utils.git build_utils"
      }
    }

    pipeDefault() {
      runStage('compile service') {
        withGithubPrivkey {
          sh "make wc_compile ${imageTags}"
        }
      }
      runStage('lint service') {
        sh "make wc_lint ${imageTags}"
      }
      runStage('check formatting for service') {
        sh "make wc_check_format ${imageTags}"
      }
      runStage('xref service') {
        sh "make wc_xref ${imageTags}"
      }
      runStage('dialyze service') {
        withWsCache("_build/test/rebar3_23.2.3_plt") {

          sh "make wc_dialyze ${imageTags}"
        }
      }
      runStage('test service') {
        sh "make wdeps_test ${imageTags}"
      }
      runStage('release service') {
        withGithubPrivkey {
          sh "make wc_release ${imageTags}"
        }
      }
    }
  }

  // erlang-library-template
  ws {
    try {
      checkoutRepo()
      loadBuildUtils()

      runStage('generate erlang library: trickster') {
        sh 'make wc_gen_library'
        sh """
        cp                                    \\
          service-templates/Dockerfile.sh     \\
          service-templates/docker-compose.sh \\
          trickster/
        """
      }

      runStage('archive trickster') {
        archive 'trickster/'
      }
    } finally {
      runStage('cleanup sub ws') {
        sh 'rm -rf * .* || echo ignore'
      }
    }
  } //close other ws

  runStage('unarchive trickster') {
    unarchive mapping: ['trickster/': '.']
    sh 'chmod 755 trickster/Dockerfile.sh trickster/docker-compose.sh'
  }

  dir('trickster') {
    runStage('init git repo') {
      sh 'git init'
      sh 'git config user.email "$CHANGE_AUTHOR_EMAIL"'
      sh 'git config user.name "$COMMIT_AUTHOR"'
      sh 'git add README.md'
      sh 'git commit -m "Initial commit"'
    }

    runStage('add git submodule') {
      withGithubSshCredentials {
        sh "git submodule add -b master git@github.com:rbkmoney/build_utils.git build_utils"
      }
    }


    pipeDefault() {
      runStage('compile library') {
        withGithubPrivkey {
          sh "make wc_compile ${imageTags}"
        }
      }
      runStage('lint library') {
        sh "make wc_lint ${imageTags}"
      }
      runStage('check formatting for library') {
        sh "make wc_check_format ${imageTags}"
      }
      runStage('xref library') {
        sh "make wc_xref ${imageTags}"
      }
      runStage('dialyze library') {
        withWsCache("_build/test/rebar3_23.2.3_plt") {
          sh "make wc_dialyze ${imageTags}"
        }
      }
      runStage('test library') {
        sh "make wdeps_test ${imageTags}"
      }
    }
  }
}
