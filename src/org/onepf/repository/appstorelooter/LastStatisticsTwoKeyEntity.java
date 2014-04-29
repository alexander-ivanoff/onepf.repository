package org.onepf.repository.appstorelooter;

import java.io.Serializable;

/**
 * Created by ivanoff on 29.04.14.
 */
public class LastStatisticsTwoKeyEntity implements Serializable {

    private String appstoreId;
    private FeedType feedType;

    public LastStatisticsTwoKeyEntity() {};

    public LastStatisticsTwoKeyEntity(String appstoreId, FeedType feedType) {
        this.appstoreId = appstoreId;
        this.feedType = feedType;
    }

    @Override
    public int hashCode() {
        return appstoreId.hashCode() + feedType.hashCode();
    }

    @Override
    public boolean equals(Object o) {
            return (o instanceof LastStatisticsTwoKeyEntity &&
                    appstoreId.equals(((LastStatisticsTwoKeyEntity)o).appstoreId) &&
                    feedType.equals(((LastStatisticsTwoKeyEntity)o).feedType));
    }
}
