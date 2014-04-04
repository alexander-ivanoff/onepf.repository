package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

/**
 * Created by ivanoff on 02.04.14.
 */
public interface ItemParser<E extends AbstractDescriptor>{


    public E getDescriptor();

    public void cleanDesciptor();

    public E removeDescriptor();

    public abstract String getElementName();
}
