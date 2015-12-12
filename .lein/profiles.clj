{:user {:jvm-opts ["-Dapple.awt.UIElement=true" "-XX:-OmitStackTraceInFastThrow"]

        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.logging "0.3.1"]
                       [org.clojure/tools.nrepl "0.2.12"]
                       [pjstadig/humane-test-output "0.7.1"]]

        :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.11.0"]
                  [jonase/eastwood "0.2.2"]
                  [lein-ancient "0.6.8"]
                  [lein-bikeshed "0.2.0"]
                  [lein-cljfmt "0.3.0"]
                  [lein-kibit "0.1.2"]
                  [lein-pdo "0.1.1"]
                  [lein-pprint "1.1.2"]
                  [refactor-nrepl "2.0.0-SNAPSHOT"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]

        :test-refresh {:quiet true}

        :aliases {"go" ["do"
                        ["clean"]
                        ["with-profiles" "+flight-recorder" "dev"]]}}

 :flight-recorder {:jvm-opts ["-XX:+UnlockCommercialFeatures" "-XX:+FlightRecorder" "-XX:FlightRecorderOptions=stackdepth=1024"]}}
