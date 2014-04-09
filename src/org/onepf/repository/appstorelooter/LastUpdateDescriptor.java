package org.onepf.repository.appstorelooter;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

/**
 * This class contains last update description:<br/>
 * appstore ID,<br/>
 * last response hash,<br/>
 * last response datetime,<br/>
 * previous offset.
 *
 * @author Alexander Ivanov
 */
public class LastUpdateDescriptor extends AbstractDescriptor {
    public String appstoreId;
    public String lastResponseHash;
    public String lastResponseDatetime;
    public String prevOffset;
}
