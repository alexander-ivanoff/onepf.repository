package org.onepf.repository.api;

/**
 * Created by akarimova on 03.04.14.
 */
public class TextUtils {
    private TextUtils() {
    }

    public static boolean isEmpty(String text) {
        return text == null || text.length() == 0;
    }

//    public Boolean getBooleanFromString(String text) {
//
//    }
}
