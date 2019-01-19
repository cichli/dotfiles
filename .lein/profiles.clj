{:user {:signing {:gpg-key "92BE8E65AB9248EFF68B75792EDA1D9FDE98B3BF"}

        :jvm-opts ["-Dapple.awt.UIElement=true" "-XX:-OmitStackTraceInFastThrow"]

        :dependencies [[criterium "0.4.4"]
                       [nrepl "0.5.3"]
                       [org.clojure/tools.logging "0.4.1"]
                       [pjstadig/humane-test-output "0.9.0"]]

        :plugins [[cider/cider-nrepl "0.20.1-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.23.0"]
                  [jonase/eastwood "0.3.4"]
                  [lein-ancient "0.6.15"]
                  [lein-bikeshed "0.5.1"]
                  [lein-cljfmt "0.6.3"]
                  [lein-kibit "0.1.6"]
                  [lein-pprint "1.2.0"]
                  [refactor-nrepl "2.4.0"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]

        :test-refresh {:quiet true}

        :aliases {"fr" ["with-profiles" "+flight-recorder"]
                  "yk" ["with-profiles" "+yourkit"]}}

 :flight-recorder {:jvm-opts ["-XX:FlightRecorderOptions=stackdepth=1024"
                              "-XX:StartFlightRecording=dumponexit=true,filename=.flight-recording.jfr,maxage=2h,settings=default"]}

 :yourkit {:jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2018.5.app/Contents/Resources/bin/mac/libyjpagent.jnilib"]
           :resource-paths ["/Applications/YourKit-Java-Profiler-2018.5.app/Contents/Resources/lib/yjp-controller-api-redist.jar"]}}
