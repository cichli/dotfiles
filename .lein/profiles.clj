{:user {:jvm-opts ["-Dapple.awt.UIElement=true" "-XX:-OmitStackTraceInFastThrow"]

        :dependencies [[criterium "0.4.4"]
                       [org.clojure/tools.logging "0.4.0"]
                       [org.clojure/tools.nrepl "0.2.13"]
                       [pjstadig/humane-test-output "0.8.3"]]

        :plugins [[cider/cider-nrepl "0.16.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.21.1"]
                  [jonase/eastwood "0.2.4"]
                  [lein-ancient "0.6.10"]
                  [lein-bikeshed "0.4.1"]
                  [lein-cljfmt "0.5.7"]
                  [lein-kibit "0.1.5"]
                  [lein-pprint "1.1.2"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]

        :test-refresh {:quiet true}

        :aliases {"fr" ["with-profiles" "+flight-recorder"]
                  "yk" ["with-profiles" "+yourkit"]}}

 :flight-recorder {:jvm-opts ["-XX:+UnlockCommercialFeatures"
                              "-XX:+FlightRecorder"
                              "-XX:FlightRecorderOptions=stackdepth=1024,defaultrecording=true,disk=true,repository=/tmp,dumponexit=true,dumponexitpath=./.flight-recording.jfr,maxage=2h,settings=jfr"]}

 :yourkit {:jvm-opts ["-agentpath:/Users/griffithsm/libyjpagent.jnilib"]
           :resource-paths ["/Users/griffithsm/yjp-controller-api-redist.jar"]}}
