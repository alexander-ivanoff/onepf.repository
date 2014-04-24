package org.onepf.repository.appstorelooter;

import org.onepf.repository.api.responsewriter.entity.BaseEntity;
import javax.persistence.*;

/**
 * Description of last update of any statistics feed.
 *
 * @author Ruslan Sayfutdinov
 */
@Entity
@Table(name = "statistics_updates")
public class LastStatisticsUpdateEntity extends BaseEntity {
    @Column(name = "appstoreId")
    @Id
    private String appstoreId;

    @Column(name = "feedType")
    @Enumerated(EnumType.STRING)
    private FeedType feedType;

    @Column(name = "lastUpdateCount")
    private int lastResponseCount;

    @Column(name = "lastUpdateDateTime")
    private String lastResponseDatetime;

    @Column(name = "lastUpdateOffset")
    private String prevOffset;

    LastStatisticsUpdateEntity(String appstoreId,
                               FeedType feedType,
                               int lastResponseCount,
                               String lastResponseDatetime,
                               String prevOffset) {
        this.appstoreId = appstoreId;
        this.feedType = feedType;
        this.lastResponseCount = lastResponseCount;
        this.lastResponseDatetime = lastResponseDatetime;
        this.prevOffset = prevOffset;
    }

    public String getAppstoreId() {
        return appstoreId;
    }

    public void setAppstoreId(String appstoreId) {
        this.appstoreId = appstoreId;
    }

    public FeedType getFeedType() { return feedType; }

    public void setFeedType(FeedType feedType) { this.feedType = feedType; }

    public int getLastResponseCount() {
        return lastResponseCount;
    }

    public void setLastResponseCount(int lastResponseCount) {
        this.lastResponseCount = lastResponseCount;
    }

    public String getLastResponseDatetime() {
        return lastResponseDatetime;
    }

    public void setLastResponseDatetime(String lastResponseDatetime) {
        this.lastResponseDatetime = lastResponseDatetime;
    }

    public String getPrevOffset() {
        return prevOffset;
    }

    public void setPrevOffset(String prevOffset) {
        this.prevOffset = prevOffset;
    }
}


