package org.onepf.repository.appstorelooter;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

/**
 * Created by ivanoff on 01.04.14.
 */
public class LastUpdateDescriptor extends AbstractDescriptor {
    public String appstoreId;
    public String lastResponseHash;
    public String lastResponseDatetime;
    public String prevOffset;
}
