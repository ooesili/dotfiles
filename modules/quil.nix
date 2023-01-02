{pkgs, ...}: {
  config.environment.etc."leiningen/profiles.clj".text = ''
    {:system
     {:resource-paths
      #=(eval (->> "${pkgs.javaPackages.jogl_2_3_2}/share/java"
                   clojure.java.io/file
                   file-seq
                   (filter #(.isFile %))
                   (map #(.getAbsolutePath %))
                   (filter #(clojure.string/ends-with? % ".jar"))
                   vec))}}
  '';
}
