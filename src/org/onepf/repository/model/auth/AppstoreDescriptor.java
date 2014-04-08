package org.onepf.repository.model.auth;

/**
 * This class contains appstore description:
 * Appstore ID,
 * Token for appstore access to repository,
 * Token for repository access to appstore,
 * Appstore description,
 * Public key for signing receipts,
 * OpenAEP URL.
 *
 * @author Alexander Ivanov
 */
public class AppstoreDescriptor {
    public String appstoreAccessToken;
    public String repositoryAccessToken;
    public String appstoreId;
    public String description;
    public String publickKey;
    public String openaepUrl;

}
