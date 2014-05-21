package org.onepf.repository.model.services;

/**
 * Created by ivanoff on 08.05.14.
 */
public enum Error {

    GENERIC(0, "Generic Error"),
    BAD_REQUEST(1, "Bad Request"),
    INTERNAL_ERROR(1, "Internal Error");

    private final int code;

    private String message;

        private Error(int errorCode, String message) {
            this.code = errorCode;
            this.message = message;
        }

        public String getMessage() {
            return message;
        }


    public int getCode() {
        return code;
    }

    public Error withMessage(String message) {
        this.message = message;
        return this;
    }
}
