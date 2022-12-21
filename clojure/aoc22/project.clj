(defproject aoc22 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [engelberg/data.union-find "1.0.0"]
                 [tarantella "1.1.1"]
                 [engelberg/data.union-find "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [ubergraph "0.8.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [rolling-stones "1.0.3"]
                 [com.rpl/specter "1.1.4"]
                 [dorothy "0.0.6"]
                 [better-cond "2.1.5"]
                 [medley "1.4.0"]
                 [instaparse "1.4.12"]
                 [loco "0.3.1"]
                 [org.clojure/core.async "1.6.673"]
                 [org.clj-commons/claypoole "1.2.2"]]
  :jvm-opts ^:replace ["-Xms1g" "-Xmx8g" "-server"]  
  :repl-options {:init-ns aoc22.core})
