package org.onepf.repository;

/**
 *
 * Mapping of available requests. Helps to construct urls to query appstores.
 *
* @author Alexander Ivanoff on 01.04.14.
 */
public enum ApiMapping {

    LIST_APPLICATIONS ("/openaep/applist"),
    LIST_PURCHASES("/openaep/purchases"),
    LIST_DOWNLOADS("/openaep/downloads"),
    LIST_REVIEWS("/openaep/reviews"),
    GET_DESCRIPTION("/openaep/appdescription"),
    GET_APPDF("/openaep/appdf"),
    UPLOAD_APPDF("/openaep/upload"),
    SIGN_PURCHASE("/openaep/signPurchase");


    private final String methodPath;

    private ApiMapping(String methodPath) {
        this.methodPath = methodPath;
    }

    /**
     * @param serverUrl root url to appstore's openaep API realization
     * @return full url to exact API method
     */
    public String getMethodUrl(String serverUrl) {
        return serverUrl + methodPath;
    }

    @Override
    public String toString() {
        return methodPath;
    }
}
