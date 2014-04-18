package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * Created by akarimova on 18.04.14.
 */
@Entity
@Table(name = "appstores")
public class AppstoreEntity {
    @Column(name = "appstoreAccessToken")
    private String appstoreAccessToken;

    @Column(name = "repositoryAccessToken")
    private String repositoryAccessToken;

    @Id
    @Column(name = "appstoreId")
    private String appstoreId;

    @Column(name = "description")
    private String description;

    @Column(name = "publicKey")
    private String publickKey;

    @Column(name = "openaepUrl")
    private String openaepUrl;

    public String getAppstoreAccessToken() {
        return appstoreAccessToken;
    }

    public void setAppstoreAccessToken(String appstoreAccessToken) {
        this.appstoreAccessToken = appstoreAccessToken;
    }

    public String getRepositoryAccessToken() {
        return repositoryAccessToken;
    }

    public void setRepositoryAccessToken(String repositoryAccessToken) {
        this.repositoryAccessToken = repositoryAccessToken;
    }

    public String getAppstoreId() {
        return appstoreId;
    }

    public void setAppstoreId(String appstoreId) {
        this.appstoreId = appstoreId;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getPublickKey() {
        return publickKey;
    }

    public void setPublickKey(String publickKey) {
        this.publickKey = publickKey;
    }

    public String getOpenaepUrl() {
        return openaepUrl;
    }

    public void setOpenaepUrl(String openaepUrl) {
        this.openaepUrl = openaepUrl;
    }
}
