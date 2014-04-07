package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.descriptors.FeatureDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by akarimova on 07.04.14.
 */
public class SqlFeatureEntity extends SqlDBEntity {
    public static final String TABLE_NAME = "feature2apk";
    public static final String FIELD_NAME = "name";
    public static final String FIELD_REQUIRED = "required";
    public static final String FIELD_GL_TEXTURE = "gl_texture";
    public static final String FIELD_APK_REFERENCE = "apk_id";

    public SqlFeatureEntity() {
        super();
    }

    public SqlFeatureEntity(Map<String, String> item) {
        super(item);
    }

    public SqlFeatureEntity withName(String name) {
        put(FIELD_NAME, name);
        return this;
    }

    public SqlFeatureEntity withRequired(boolean required) {
        put(FIELD_REQUIRED, required ? 1 : 0);
        return this;
    }

    public SqlFeatureEntity withGLTexture(int glTexture) {
        put(FIELD_GL_TEXTURE, glTexture);
        return this;
    }

    public SqlFeatureEntity withApkReference(int apkReference) {
        put(FIELD_APK_REFERENCE, apkReference);
        return this;
    }

    public static FeatureDescriptor getDescriptor(ResultSet item) throws SQLException {
        FeatureDescriptor featureDescriptor = new FeatureDescriptor();
        featureDescriptor.setName(item.getString(FIELD_NAME));
        featureDescriptor.setRequired(item.getInt(FIELD_REQUIRED) == 1);
        featureDescriptor.setGlEsVersion(item.getInt(FIELD_GL_TEXTURE));
        featureDescriptor.setApkId(item.getInt(FIELD_APK_REFERENCE));
        return featureDescriptor;
    }
}
