package org.onepf.repository;

/**
 * Created by ivanoff on 01.04.14.
 */
public enum ApiMapping {

    LIST_APPLICATIONS ("/openaep/applist"),
    LIST_PURCHASES("/openaep/purchases"),
    LIST_DOWNLOADS("/openaep/downloads"),
    LIST_REVIEWS("/openaep/reviews"),
    GET_DESCRIPTION("/openaep/appdescription"),
    GET_APPDF("/openaep/appdf"),
    UPLOAD_APPDF("/openaep/upload");


    private final String methodPath;

    ApiMapping(String methodPath) {
        this.methodPath = methodPath;
    }

    public String getMethodUrl(String serverUrl) {
        return serverUrl + methodPath;
    }

    @Override
    public String toString() {
        return methodPath;
    }
}
