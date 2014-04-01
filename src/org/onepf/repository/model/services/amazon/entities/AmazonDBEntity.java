package org.onepf.repository.model.services.amazon.entities;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import org.onepf.repository.model.services.DBEntity;
import org.onepf.repository.utils.responsewriter.descriptors.AbstractDescriptor;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public abstract class AmazonDBEntity<T extends AbstractDescriptor> extends DBEntity<T, Map<String, AttributeValue>, Map<String, AttributeValue>> {

    protected Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();

    public AmazonDBEntity(String tableName) {
        super(tableName);
    }

    public AmazonDBEntity(String tableName, Map<String, AttributeValue> item) {
        super(tableName);
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
