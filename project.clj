(defproject usng3 "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.apache.commons/commons-lang3 "3.5"]
                 [junit/junit "4.12"]]
  :plugins [[lein-junit "1.1.8"]
            [lein-npm "0.6.2"]
            [lein-cljsbuild "1.1.7"]]
  :npm {:devDependencies [[mocha "^5.2.0"]
                          [chai "^4.1.2"]]
        :package         {:scripts {:test "mocha test/js/*.js"}}}
  :cljsbuild
  {:builds [{:source-paths ["src/clojure"]
             :compiler     {:externs       ["src/js/externs.js"]
                            :output-to     "target/usng/index.js"
                            :optimizations :advanced}}]}
  :aot :all
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :junit ["test/java"]
  :profiles {:junit {:java-source-paths ["test/java"]
                     :source-paths      ["src/java" "target/classes"]}}
  :aliases {"test" ["do"
                    ["clean"]
                    ["cljsbuild" "once"]
                    ["test"]
                    ["npm" "test"]
                    ["compile"]
                    ["with-profile" "+junit" "javac"]
                    ["with-profile" "+junit" "junit"]]})
