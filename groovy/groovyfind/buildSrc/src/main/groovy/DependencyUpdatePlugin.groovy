import org.gradle.api.Plugin
import org.gradle.api.Project

import java.security.MessageDigest
import java.time.Instant

class DependencyUpdatePlugin implements Plugin<Project> {
    void apply(Project project) {
        project.tasks.register("checkForDependencyUpdates") {
            doLast {
                def cacheDir = new File(project.buildDir, "dependencyUpdateCache")
                def cacheTtlMinutes = 60L

                project.configurations.each { config ->
                    config.dependencies.each { dep ->
                        def group = dep.group
                        def artifact = dep.name
                        def version = dep.version

                        if (!group || !version) {
                            return
                        }

                        def groupPath = group.replace('.', '/')
                        def metadataUrl = "https://repo1.maven.org/maven2/${groupPath}/${artifact}/maven-metadata.xml"

                        // Hash the cache key to create a safe filename
                        def cacheKey = "${group}:${artifact}"
                        def md5 = MessageDigest.getInstance("MD5").digest(cacheKey.bytes)
                        def hash = md5.collect { String.format("%02x", it) }.join()

                        def cacheFile = new File(cacheDir, "${hash}.xml")
                        def now = Instant.now().epochSecond

                        def xml
                        if (cacheFile.exists() && now - (cacheFile.lastModified() / 1000) < cacheTtlMinutes * 60) {
                            xml = cacheFile.text
                        } else {
                            try {
                                xml = new URL(metadataUrl).text
                                cacheFile.parentFile.mkdirs()
                                cacheFile.text = xml
                            } catch (Exception e) {
                                println "Failed to fetch metadata for ${group}:${artifact}: ${e.message}"
                                return
                            }
                        }

                        def matcher = (xml =~ /<latest>(.*?)<\/latest>/)
                        if (matcher.find()) {
                            def latest = matcher.group(1)
                            if (latest != version) {
                                println "Update available: ${group}:${artifact} ${version} -> ${latest}"
                            }
                        }
                    }
                }
            }
        }
    }
}
