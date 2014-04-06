package org.onepf.repository.utils.uploader.utils;

/**
 * Created by akarimova on 03.04.14.
 */
public class Screen implements TbdInteface {
    private String screenSize;
    private String screenDensity;

    public String getScreenSize() {
        return screenSize;
    }

    public void setScreenSize(String screenSize) {
        this.screenSize = screenSize;
    }

    public String getScreenDensity() {
        return screenDensity;
    }

    public void setScreenDensity(String screenDensity) {
        this.screenDensity = screenDensity;
    }

    @Override
    public String stringValue() {
        return null;
    }

    @Override
    public void setUpFromString(String source) {

    }
}
