package org.onepf.repository.appstorelooter;

/**
 * Feed type enum.
 *
 * @author  Ruslan Sayfutdinov
 */
public enum FeedType {
    DOWNLOADS("downloads"),
    REVIEWS("reviews"),
    PURCHASES("purchases");

    private final String type;

    FeedType(String type) {
        this.type = type;
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
}
