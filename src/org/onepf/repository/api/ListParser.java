package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

import java.util.Collection;

/**
 *
 * Interface to parse list of objects into collection and some generic data into header object
 *
 * @author Alexander Ivanoff on 01.04.14.
 */
public interface ListParser<T extends Object, K extends Object> {

    /**
     * @return collection of parsed objects
     */
    public Collection<T> getItems();

    /**
     * @return some generic information about list stored in one header object
     */
    public K getHeader();


}
