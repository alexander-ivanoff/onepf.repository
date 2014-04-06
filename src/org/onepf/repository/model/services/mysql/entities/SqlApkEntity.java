package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.descriptors.ApkDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by akarimova on 06.04.14.
 */
public class SqlApkEntity extends SqlDBEntity {
    public static final String TABLE_NAME = "apks";
    public static final String FIELD_PACKAGE_NAME = "package_name";
    public static final String FIELD_SHA1 = "sha1";
    public static final String FIELD_VERSION_CODE = "version_code";
    public static final String FIELD_VERSION_NAME = "version_name";
    public static final String FIELD_MIN_SDK = "min_sdk";
    public static final String FIELD_TARGET_SDK = "target_sdk";
    public static final String FIELD_MAX_SDK = "max_sdk";

    public SqlApkEntity() {
        super();
    }

    public SqlApkEntity(Map<String, String> item) {
        super(item);
    }

    public SqlApkEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public SqlApkEntity withSha1(String sha1) {
        put(FIELD_SHA1, sha1);
        return this;
    }

    public SqlApkEntity withVersionCode(String versionCode) {
        put(FIELD_VERSION_CODE, versionCode);
        return this;
    }

    public SqlApkEntity withVersionName(String versionName) {
        put(FIELD_VERSION_NAME, versionName);
        return this;
    }

    public SqlApkEntity withMinSdk(String minSdk) {
        put(FIELD_MIN_SDK, minSdk);
        return this;
    }

    public SqlApkEntity withTargetSdk(String targetSdk) {
        put(FIELD_TARGET_SDK, targetSdk);
        return this;
    }

    public SqlApkEntity withMaxSdk(String maxSdk) {
        put(FIELD_MAX_SDK, maxSdk);
        return this;
    }

    public static ApkDescriptor getDescriptor(ResultSet item) throws SQLException {
        ApkDescriptor apk = new ApkDescriptor();
        apk.setPackageName(item.getString(FIELD_PACKAGE_NAME));
        apk.setSha1(item.getString(FIELD_SHA1));
        apk.setVersionCode(Integer.valueOf(item.getString(FIELD_VERSION_CODE)));
        apk.setVersionName(item.getString(FIELD_VERSION_NAME));
        apk.setMinSdk(Integer.valueOf(item.getString(FIELD_MIN_SDK)));
        apk.setTargetSdk(Integer.valueOf(item.getString(FIELD_TARGET_SDK)));
        apk.setMaxSdk(Integer.valueOf(item.getString(FIELD_MAX_SDK)));
        return apk;
    }
}
