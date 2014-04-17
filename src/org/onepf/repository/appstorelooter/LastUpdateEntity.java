package org.onepf.repository.appstorelooter;

import org.onepf.repository.api.responsewriter.entity.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Created by akarimova on 17.04.14.
 */
@Entity
@Table(name = "appstoreupdates")
public class LastUpdateEntity extends BaseEntity {
    @Column(name = "appstoreId")
    private String appstoreId;

    @Column(name = "lastUpdateHash")
    private String lastResponseHash;

    @Column(name = "lastUpdateDateTime")
    private String lastResponseDatetime;

    @Column(name = "lastUpdateOffset")
    private String prevOffset;

    public String getAppstoreId() {
        return appstoreId;
    }

    public void setAppstoreId(String appstoreId) {
        this.appstoreId = appstoreId;
    }

    public String getLastResponseHash() {
        return lastResponseHash;
    }

    public void setLastResponseHash(String lastResponseHash) {
        this.lastResponseHash = lastResponseHash;
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
