package org.onepf.repository.model.services;

import org.onepf.repository.utils.responsewriter.descriptors.AbstractDescriptor;

/**
 * Created by ivanoff on 01.04.14.
 *
 * T - is concrete Descriptor of entity
 * E - is source where we take data from
 * K - is representation to store data
 */
public abstract class DBEntity<T extends AbstractDescriptor, E, K> {

    private String tableName;

    public DBEntity(String tableName) {
        super();
        this.tableName = tableName;
    }

    public String getTableName() {
        return tableName;
    }

    abstract public T getDescriptor(E source) throws DataException;
    abstract public K getItem();

}
