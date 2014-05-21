package org.onepf.repository.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by ivanoff on 07.05.14.
 */
public class Utils {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");


    public static String sqlFormattedDate(Date date) {
        return dateFormat.format(date);
    }
}
