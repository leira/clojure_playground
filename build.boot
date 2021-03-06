(def project 'playground)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.8.0"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [com.taoensso/tufte "1.1.2"]])

; (task-options!
;  aot {:namespace   #{'playground.core}}
;  pom {:project     project
;       :version     version
;       :description "FIXME: write description"
;       :url         "http://example/FIXME"
;       :scm         {:url "https://github.com/yourname/playground"}
;       :license     {"Eclipse Public License"
;                     "http://www.eclipse.org/legal/epl-v10.html"}}
;  jar {:main        'playground.core
;       :file        (str "playground-" version "-standalone.jar")})
; 
; (deftask build
;   "Build the project locally as a JAR."
;   [d dir PATH #{str} "the set of directories to write to (target)."]
;   (let [dir (if (seq dir) dir #{"target"})]
;     (comp (aot) (pom) (uber) (jar) (target :dir dir))))
; 
; (deftask run
;   "Run the project."
;   [a args ARG [str] "the arguments for the application."]
;   (require '[playground.core :as app])
;   (apply (resolve 'app/-main) args))

(require '[adzerk.boot-test :refer [test]])
