package org.onepf.repository.model;

/**
 *
 * Helper class to wrap file types we work with.
 *
* @author Alexander Ivanoff on 19.03.14.
 */
public enum FileType {

    APK ("apk"),
    APPDF("appdf"),
    DESCRIPTION("xml");


    private final String extension;

    private FileType(String extension) {
        this.extension = extension;
    }

    /**
     * @return extension of this file type
     */
    public String extension() {
        return extension;
    }

    /**
     * @param name - name of the file
     * @return name of a file with extension
     */
    public String addExtension(String name) {
        return name + '.' + extension;
    }
}
