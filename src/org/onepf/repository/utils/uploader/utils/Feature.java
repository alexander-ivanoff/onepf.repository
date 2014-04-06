package org.onepf.repository.utils.uploader.utils;

/**
 * Created by akarimova on 03.04.14.
 */
public class Feature implements TbdInteface {
    private String name;
    private int glEsVersion;
    private boolean required;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getGlEsVersion() {
        return glEsVersion;
    }

    public void setGlEsVersion(int glEsVersion) {
        this.glEsVersion = glEsVersion;
    }

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public String stringValue() {
        return String.format("%s,%b,%d", name, required, glEsVersion);
    }

    public void setUpFromString(String source) {
        String[] split = source.split(",");
        if (split.length != 3) {
            throw new IllegalStateException("Feature must contains 3 parts: name, required, glTexture");
        }
        name = split[0];
        required = Boolean.valueOf(split[1]);
        glEsVersion = Integer.valueOf(split[2]);
    }

    @Override
    public String toString() {
        return "Feature{" +
                "name='" + name + '\'' +
                ", glEsVersion=" + glEsVersion +
                ", required=" + required +
                '}';
    }
}
