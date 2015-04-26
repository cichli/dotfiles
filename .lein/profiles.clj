{:user {:jvm-opts ["-Dapple.awt.UIElement=true" "-XX:-OmitStackTraceInFastThrow"]
        :dependencies [[criterium "0.4.3"]
                       [pjstadig/humane-test-output "0.7.0"]]
        :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.7.0"]
                  [jonase/eastwood "0.2.1"]
                  [lein-ancient "0.6.7"]
                  [lein-bikeshed "0.2.0"]
                  [lein-cljfmt "0.1.10"]
                  [lein-kibit "0.0.8"]
                  [refactor-nrepl "1.0.1"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
