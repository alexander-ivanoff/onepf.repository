package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

import java.util.Collection;

/**
 * Created by ivanoff on 01.04.14.
 */
public interface ListParser<T extends AbstractDescriptor, K extends  AbstractDescriptor> {

    public Collection<T> getItems();
    public K getHeader();


}
