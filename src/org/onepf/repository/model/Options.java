package org.onepf.repository.model;

import java.util.HashMap;

/**
 * Created by ivanoff on 20.03.14.
 */
public class Options extends HashMap<String, String> {

    public static final String PACKAGE_NAME = "package";
    public static final String UPDATE_TIME = "updateTime";

    public Options() {
        super();
    }

    public Options(int size) {
        super(size);
    }


}
