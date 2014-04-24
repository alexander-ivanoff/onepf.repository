package org.onepf.repository.appstorelooter;

import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.entity.*;

/**
 * Feed type enum.
 *
 * @author  Ruslan Sayfutdinov
 */
public enum FeedType {
    DOWNLOADS("downloads", ApiMapping.LIST_DOWNLOADS, DownloadListEntity.class, DownloadEntity.class),
    REVIEWS("reviews", ApiMapping.LIST_REVIEWS, ReviewsListEntity.class, ReviewEntity.class),
    PURCHASES("purchases", ApiMapping.LIST_PURCHASES, PurchaseListEntity.class, PurchaseEntity.class);

    private final String type;
    private final ApiMapping apiMapping;
    private final Class listEntity;
    private final Class elementEntity;

    FeedType(String type, ApiMapping mapping, Class listEntity, Class elementEntity) {
        this.type = type;
        this.apiMapping = mapping;
        this.listEntity = listEntity;
        this.elementEntity = elementEntity;
    }

    public static FeedType fromString(String feedType) {
        for (FeedType type : FeedType.values()) {
            if (feedType.equals(type.type)) {
                return type;
            }
        }
        throw new IllegalArgumentException("No FeedType with type " + feedType + " found");
    }

    @Override
    public String toString() {
        return type;
    }

    public ApiMapping getApiMapping() { return apiMapping; }

    public Class getListEntity() { return listEntity; }

    public Class getElementEntity() { return elementEntity; }
}
