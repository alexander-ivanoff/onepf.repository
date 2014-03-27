package org.onepf.repository.model.services.mysql.entities;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlDBEntity {

    protected Map<String, String> item = new HashMap<String, String>();

    public SqlDBEntity() {
        super();
    }

    public SqlDBEntity(Map<String, String> item) {
        super();
        this.item = item;
    }


    protected SqlDBEntity put(String field, String value) {
        item.put(field, value);
        return this;
    }

    protected SqlDBEntity put(String field, long value) {
        item.put(field, String.valueOf(value));
        return this;
    }

    protected SqlDBEntity put(String field, Collection<String> values) {
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