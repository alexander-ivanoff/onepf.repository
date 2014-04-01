package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.model.services.DBEntity;
import org.onepf.repository.utils.responsewriter.descriptors.AbstractDescriptor;

import java.sql.ResultSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public abstract class SqlDBEntity<T extends  AbstractDescriptor> extends DBEntity<T, ResultSet, Map<String, String>> {

    protected Map<String, String> item = new HashMap<String, String>();

    public SqlDBEntity(String tableName) {
        super(tableName);
    }



    protected SqlDBEntity<T> put(String field, String value) {
        item.put(field, value);
        return this;
    }

    protected SqlDBEntity<T> put(String field, long value) {
        item.put(field, String.valueOf(value));
        return this;
    }

    protected SqlDBEntity<T> put(String field, Collection<String> values) {
        item.put(field, values.toString());
        return this;
    }


    protected String getString(String field) {
        return getString(item, field);
    }

    protected long getLong(String field) {
        return getLong(item, field);
    }

    protected int getInt(String field) {
        return getInt(item, field);
    }

    protected static String getString(Map<String, String> item, String field) {
        return item.get(field);
    }

    protected static long getLong(Map<String, String> item, String field) {
        return Long.valueOf(item.get(field));
    }

    protected static int getInt(Map<String, String> item, String field) {
        return Integer.valueOf(item.get(field));
    }

    public Map<String, String> getItem() {
        return item;
    }

}
