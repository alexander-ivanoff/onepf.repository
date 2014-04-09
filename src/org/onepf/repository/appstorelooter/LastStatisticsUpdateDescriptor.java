package org.onepf.repository.appstorelooter;

import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;

/**
 * Description of last update of any statistics feed.
 *
 * @author Ruslan Sayfutdinov
 */
public class LastStatisticsUpdateDescriptor extends AbstractDescriptor {
    public String appstoreId;
    public FeedType feedType;
    public String lastResponseCount;
    public String lastResponseDatetime;
    public String prevOffset;
}
