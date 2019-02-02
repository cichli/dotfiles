{:user {:signing {:gpg-key "92BE8E65AB9248EFF68B75792EDA1D9FDE98B3BF"}

        :jvm-opts ["-Dapple.awt.UIElement=true" "-XX:-OmitStackTraceInFastThrow"]

        :dependencies [[criterium "0.4.4"]
                       [nrepl "0.5.3"]
                       [org.clojure/tools.logging "0.4.1"]
                       [pjstadig/humane-test-output "0.9.0"]]

        :plugins [[cider/cider-nrepl "0.20.1-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.23.0"]
                  [jonase/eastwood "0.3.5"]
                  [lein-ancient "0.6.15"]
                  [lein-bikeshed "0.5.1"]
                  [lein-cljfmt "0.6.4"]
                  [lein-kibit "0.1.6"]
                  [lein-nvd "0.6.0"]
                  [lein-pprint "1.2.0"]
                  [refactor-nrepl "2.4.0"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}

 :frs [:flight-recorder :flight-recorder/sampling]
 :frt [:flight-recorder :flight-recorder/tracing]

 :flight-recorder {:jvm-opts ["-XX:+UnlockDiagnosticVMOptions"
                              "-XX:+DebugNonSafepoints"
                              "-XX:FlightRecorderOptions=stackdepth=1024"]}

 :flight-recorder/sampling {:jvm-opts ["-XX:StartFlightRecording=dumponexit=true,filename=.flight-recording.jfr,maxage=2h,settings=default"]}
 :flight-recorder/tracing  {:jvm-opts ["-XX:StartFlightRecording=dumponexit=true,filename=.flight-recording.jfr,maxage=2h,settings=profile"]}

 :yk  [:yourkit/agent :yourkit/api]
 :yks [:yourkit/sampling]
 :ykt [:yourkit/tracing]

 :yourkit/agent {:jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2019.1.app/Contents/Resources/bin/mac/libyjpagent.jnilib"]}
 :yourkit/api   {:resource-paths ["/Applications/YourKit-Java-Profiler-2019.1.app/Contents/Resources/lib/yjp-controller-api-redist.jar"]}

 :yourkit/sampling {:jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2019.1.app/Contents/Resources/bin/mac/libyjpagent.jnilib=sampling"]}
 :yourkit/tracing  {:jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2019.1.app/Contents/Resources/bin/mac/libyjpagent.jnilib=tracing"]}}
