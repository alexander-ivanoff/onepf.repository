package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.appstorelooter.FeedType;
import org.onepf.repository.appstorelooter.LastStatisticsUpdateDescriptor;
import org.onepf.repository.appstorelooter.LastUpdateDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Last statistics update entity for MySQL.
 * Create LastStatisticsUpdateDescriptor from ResultSet.
 *
 * @see org.onepf.repository.appstorelooter.LastStatisticsUpdateDescriptor
 * @author Ruslan Sayfutdinov
 */
public class SqlLastStatisticsUpdateEntity extends SqlDBEntity {


    public static final String TABLE_NAME = "statistics_updates";

    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_FEED_TYPE = "feedType";
    public static final String FIELD_LAST_UPDATE_COUNT= "lastUpdateCount";
    public static final String FIELD_LAST_UPDATE_DATETIME = "lastUpdateDateTime";
    public static final String FIELD_LAST_UPDATE_OFFSET = "lastUpdateOffset";


    public SqlLastStatisticsUpdateEntity() {
        super();
    }

    public SqlLastStatisticsUpdateEntity(Map<String, String> item) {
        super(item);
    }


    public SqlLastStatisticsUpdateEntity withAppstoreId(String appstoreId) {
        put(FIELD_APPSTORE_ID, appstoreId);
        return this;
    }

    public SqlLastStatisticsUpdateEntity withFeedType(String feedType) {
        put(FIELD_FEED_TYPE, feedType);
        return this;
    }

    public SqlLastStatisticsUpdateEntity withCount(int count) {
        put(FIELD_LAST_UPDATE_COUNT, count);
        return this;
    }

    public SqlLastStatisticsUpdateEntity withDateTime(String datetime) {
        put(FIELD_LAST_UPDATE_DATETIME, datetime);
        return this;
    }

    public SqlLastStatisticsUpdateEntity withOffset(String offset) {
        put(FIELD_LAST_UPDATE_OFFSET, offset);
        return this;
    }


    public static LastStatisticsUpdateDescriptor getDescriptor(ResultSet item) throws SQLException {
        LastStatisticsUpdateDescriptor lastStatisticsUpdateDescriptor = new LastStatisticsUpdateDescriptor();
        lastStatisticsUpdateDescriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        lastStatisticsUpdateDescriptor.feedType = FeedType.fromString(item.getString(FIELD_FEED_TYPE));
        lastStatisticsUpdateDescriptor.lastResponseCount = item.getString(FIELD_LAST_UPDATE_COUNT);
        lastStatisticsUpdateDescriptor.lastResponseDatetime = item.getString(FIELD_LAST_UPDATE_DATETIME);
        lastStatisticsUpdateDescriptor.prevOffset = item.getString(FIELD_LAST_UPDATE_OFFSET);
        return lastStatisticsUpdateDescriptor;
    }


}
