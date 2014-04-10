package org.onepf.repository.api.xmlapi;

/**
 * Support class contains element and field names for different requests
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public interface XMLElements {

    public static interface Application {
        public static final String ELEMENT_NAME = "application";

        public static final String FIELD_PACKAGE = "package";
        public static final String FIELD_LAST_UPDATED = "last-updated";
        public static final String FIELD_HASH = "md5-hash";
    }

    public static interface BaseHeader {
        public static final String FIELD_VERSION = "version";
        public static final String FIELD_OFFSET = "offset";
    }

    public static interface ApplicationListHeader extends BaseHeader {
        public static final String ELEMENT_NAME = "application-list";
    }

    public static interface DownloadsListHeader  extends BaseHeader{
        public static final String ELEMENT_NAME = "downloads";
    }

    public static interface PurchasesListHeader extends BaseHeader{
        public static final String ELEMENT_NAME = "purchases";
    }

    public static interface ReviewsListHeader extends BaseHeader{
        public static final String ELEMENT_NAME = "reviews";
    }
}
