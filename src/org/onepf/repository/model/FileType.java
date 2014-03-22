package org.onepf.repository.model;

/**
 * Created by ivanoff on 19.03.14.
 */
public enum FileType {

    APK ("apk"),
    APPDF("appdf"),
    DESCRIPTION("xml");


    private final String extention;

    FileType(String extention) {
        this.extention = extention;
    }

    public String extention() {
        return  extention;
    }

    public String addExtention(String name) {
        return name + '.' + extention;
    }
}
