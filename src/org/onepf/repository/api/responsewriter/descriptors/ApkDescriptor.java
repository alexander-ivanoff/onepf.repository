package org.onepf.repository.api.responsewriter.descriptors;

import java.io.File;
import java.io.FileInputStream;
import java.security.MessageDigest;
import java.util.*;

/**
 * Created by akarimova on 04.04.14.
 */
public class ApkDescriptor extends AbstractDescriptor {
    public static final int EQUAL = 0;
    public static final int LESS = -1;
    public static final int MORE = 1;
    public static final int DIFFERENT = 2;

    private int versionCode;
    private String versionName;
    private int minSdk;
    private int maxSdk;
    private int targetSdk;
    private String packageName;
    private String sha1;
    private Set<String> features = new HashSet<String>();

    public int getVersionCode() {
        return versionCode;
    }

    public void setVersionCode(int versionCode) {
        this.versionCode = versionCode;
    }

    public String getVersionName() {
        return versionName;
    }

    public void setVersionName(String versionName) {
        this.versionName = versionName;
    }

    public int getMinSdk() {
        return minSdk;
    }

    public void setMinSdk(int minSdk) {
        this.minSdk = minSdk;
    }

    public int getMaxSdk() {
        return maxSdk;
    }

    public void setMaxSdk(int maxSdk) {
        this.maxSdk = maxSdk;
    }

    public int getTargetSdk() {
        return targetSdk;
    }

    public void setTargetSdk(int targetSdk) {
        this.targetSdk = targetSdk;
    }

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public Set<String> getFeatures() {
        return features;
    }

    public void setFeatures(Set<String> features) {
        this.features = features;
    }

    public String getSha1() {
        return sha1;
    }

    public void setSha1(String sha1) {
        this.sha1 = sha1;
    }

    public int compareApk(ApkDescriptor otherApk) {
        if (getVersionCode() == otherApk.getVersionCode()) {
            throw new IllegalStateException("2 apks have the same version code " + getVersionCode());
        }
        //features by name  only
        int compareFeaturesResult;
        boolean f2inf1 = getFeatures().containsAll(otherApk.getFeatures());
        boolean f1inf2 = otherApk.getFeatures().containsAll(getFeatures());
        if (f2inf1 && f1inf2) {
            compareFeaturesResult = 0;
        } else if (f2inf1) {
            compareFeaturesResult = -1;
        } else {
            compareFeaturesResult = 1;
        }
        //int compareMinSdkResult = Integer.valueOf(versionCode).compareTo(otherApk.versionCode);
        int compareMinSdkResult = compareSdk(getMinSdk(), otherApk.getMinSdk());
//        int compareTargetSdkResult = compareSdk(targetSdk, otherApk.targetSdk); //todo it it required?
        int compareMaxSdkResult = compareSdk(getMaxSdk(), otherApk.getMaxSdk());
        return compareFeaturesResult;
    }

    private static int compareSdk(int sdk1, int sdk2) {
        int compareSdkResult;
        if (sdk1 == sdk2) {
            compareSdkResult = 0;
        } else if (sdk1 > sdk2) {
            compareSdkResult = 1;
        } else {
            compareSdkResult = -1;
        }
        return compareSdkResult;
    }

    public static String sha1(File file) throws Exception {
        MessageDigest md = MessageDigest.getInstance("SHA1");
        FileInputStream fis = new FileInputStream(file);
        byte[] dataBytes = new byte[1024];
        int nread;
        while ((nread = fis.read(dataBytes)) != -1) {
            md.update(dataBytes, 0, nread);
        }
        byte[] mdbytes = md.digest();
        StringBuilder sb = new StringBuilder("");
        for (byte mdbyte : mdbytes) {
            sb.append(Integer.toString((mdbyte & 0xff) + 0x100, 16).substring(1));
        }
        return sb.toString();
    }
}
