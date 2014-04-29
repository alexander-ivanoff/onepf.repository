package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * Created by akarimova on 16.04.14.
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "reviewType")
@Entity
@Table(name = "reviews")
public class ReviewEntity extends BaseStatisticEntity {

    @XmlAttribute(name = "rating", required = true)
    @Column(name = "rating")
    private float rating;

    @XmlAttribute(name = "user-name")
    @Column(name = "userName")
    private String userName;

    @XmlAttribute(name = "review-url")
    @Column(name = "reviewUrl")
    private String userUrl;

    @XmlAttribute(name = "title")
    @Column(name = "title")
    private String title;

    @XmlAttribute(name = "text")
    @Column(name = "textBody")
    private String body;

    public float getRating() {
        return rating;
    }

    public void setRating(float rating) {
        this.rating = rating;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserUrl() {
        return userUrl;
    }

    public void setUserUrl(String userUrl) {
        this.userUrl = userUrl;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

}
