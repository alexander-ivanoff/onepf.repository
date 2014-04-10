package org.onepf.repository.model.services.amazon.entities;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
* @author Alexander Ivanoff on 18.03.14.
 */
public class AmazonDBEntity {

    protected Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();

    public AmazonDBEntity() {
        super();
    }

    public AmazonDBEntity(Map<String, AttributeValue> item) {
        super();
        this.item = item;
    }


    protected AmazonDBEntity put(String field, String value) {
        item.put(field, new AttributeValue().withS(value));
        return this;
    }

    protected AmazonDBEntity put(String field, long value) {
        item.put(field, new AttributeValue().withN(String.valueOf(value)));
        return this;
    }

    protected AmazonDBEntity put(String field, Collection<String> values) {
        item.put(field, new AttributeValue().withSS(values));
        return this;
    }

    protected AmazonDBEntity put(String field, AttributeValue value) {
        item.put(field, value);
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

    protected List<String> getStringList(String field) {
        return getStringList(item, field);
    }

    protected static String getString(Map<String, AttributeValue> item, String field) {
        AttributeValue value = item.get(field);
        if (value != null) {
            return value.getS();
        }
        return null;
    }

    protected static long getLong(Map<String, AttributeValue> item, String field) {
        AttributeValue value = item.get(field);
        if (value != null) {
            return Long.valueOf(value.getN());
        }
        return 0;
    }

    protected static int getInt(Map<String, AttributeValue> item, String field) {
        AttributeValue value = item.get(field);
        if (value != null) {
            return Integer.valueOf(value.getN());
        }
        return 0;
    }

    protected static List<String> getStringList(Map<String, AttributeValue> item, String field) {
        AttributeValue value = item.get(field);
        if (value != null) {
            return value.getSS();
        }
        return null;
    }


    public Map<String, AttributeValue> getItem() {
        return item;
    }
}
