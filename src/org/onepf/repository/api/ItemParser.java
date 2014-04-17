package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;
import org.onepf.repository.api.responsewriter.entity.BaseEntity;

/**
 *
 * Interface to parse single objects
 *
 * @author Alexander Ivanoff on 01.04.14.
 */
public interface ItemParser<E extends BaseEntity>{

    /**
     * @return parsed object
     */
    public E getDescriptor();

    /**
     * delete information about parsed object in parser
     */
    public void cleanDesciptor();

    /**
     * @return parsed object, and delete information about parsed object in parser
     */
    public E removeDescriptor();

    /**
     * @return name of the parsed element
     */
    public String getElementName();
}
