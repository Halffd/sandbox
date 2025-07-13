package com.half.javalearning.classes.interfaces;

import java.util.*;

// Base interfaces for the chaos
interface Cookable {
    void cook();
    boolean isBurnt();
    void makeItSpicy();
}

interface Servable {
    void serve();
    void yeetToCustomer();
    boolean isStillEdible();
}

interface Memeworthy {
    void addMemeSeasoningTM();
    String generateTikTokCaption();
    void makeItViral();
}

interface CustomerComplaint {
    void karen();
    void demandManager();
    void oneStarReview();
}

interface SocialMediaInfluencer {
    void takeFoodPics();
    void complain_about_lighting();
    void askForFreeMeal();
}

interface GordonRamsayMode {
    void roastTheChef();
    void throwPlateAcrossKitchen();
    void yellIdiotSandwich();
}

// The absolute unit of a meme restaurant
class ChaosKitchenRestaurant implements Cookable, Servable, Memeworthy, GordonRamsayMode {
    private String dishName;
    private boolean burnt = false;
    private boolean viral = false;
    private int spiceLevel = 0;
    private List<String> memeIngredients;

    public ChaosKitchenRestaurant(String dishName) {
        this.dishName = dishName;
        this.memeIngredients = new ArrayList<>();
        memeIngredients.add("Salt Bae seasoning");
        memeIngredients.add("Among Us sauce");
        memeIngredients.add("Shrek onions");
    }

    @Override
    public void cook() {
        System.out.println("👨‍🍳 Cooking " + dishName + " while playing lo-fi hip hop...");
        if (Math.random() > 0.7) {
            burnt = true;
            System.out.println("🔥 BRUH IT'S BURNT! Time to blame the oven...");
        }
    }

    @Override
    public boolean isBurnt() {
        return burnt;
    }

    @Override
    public void makeItSpicy() {
        spiceLevel += 9000;
        System.out.println("🌶️ IT'S OVER 9000!!! Added Carolina Reaper just for the memes");
    }

    @Override
    public void serve() {
        System.out.println("🍽️ Serving " + dishName + " with extra ✨aesthetic✨");
        if (burnt) {
            System.out.println("📝 *whispers* Tell them it's 'rustic style'");
        }
    }

    @Override
    public void yeetToCustomer() {
        System.out.println("🚀 YEET! Flying " + dishName + " incoming!");
        System.out.println("💥 *plate crashes* 'That'll be $47.99 plus tip'");
    }

    @Override
    public boolean isStillEdible() {
        return !burnt && spiceLevel < 10000;
    }

    @Override
    public void addMemeSeasoningTM() {
        System.out.println("🧂 Adding forbidden seasoning...");
        for (String ingredient : memeIngredients) {
            System.out.println("   ➕ Sprinkling " + ingredient);
        }
        System.out.println("🎭 *chef's kiss* Perfection!");
    }

    @Override
    public String generateTikTokCaption() {
        return "POV: You're at the most chaotic restaurant 😭💀 #foodie #chaos #help";
    }

    @Override
    public void makeItViral() {
        viral = true;
        System.out.println("📱 Posted on TikTok, Instagram, and accidentally sent to mom");
        System.out.println("📈 STONKS! We're trending!");
    }

    @Override
    public void roastTheChef() {
        System.out.println("👨‍🍳💢 WHERE'S THE LAMB SAUCE?!");
        System.out.println("🤬 THIS FOOD IS SO BAD, EVEN THE FLIES LEFT!");
        System.out.println("😡 YOU DONUT! YOU MUPPET!");
    }

    @Override
    public void throwPlateAcrossKitchen() {
        System.out.println("🍽️💨 *CRASH* 💥");
        System.out.println("🧹 Intern crying in the corner...");
    }

    @Override
    public void yellIdiotSandwich() {
        System.out.println("🥪 WHAT ARE YOU?!");
        System.out.println("😰 An... idiot sandwich...");
        System.out.println("👨‍🍳 YES YOU ARE!");
    }
}

// Karen Customer class (because every restaurant needs one)
class KarenCustomer implements CustomerComplaint, SocialMediaInfluencer {
    private String name = "Karen";
    private boolean satisfied = false;

    @Override
    public void karen() {
        System.out.println("👩‍💼 EXCUSE ME! This is NOT what I ordered!");
        System.out.println("😤 I have been coming here for 2 minutes and this is UNACCEPTABLE!");
    }

    @Override
    public void demandManager() {
        System.out.println("🗣️ I WANT TO SPEAK TO THE MANAGER RIGHT NOW!");
        System.out.println("👨‍💼 *manager hides in freezer*");
    }

    @Override
    public void oneStarReview() {
        System.out.println("⭐ 1/5 stars on Yelp");
        System.out.println("📝 'Terrible service, my latte art wasn't perfect'");
        System.out.println("🤳 *posts 47 angry Facebook comments*");
    }

    @Override
    public void takeFoodPics() {
        System.out.println("📸 *takes 127 photos of salad*");
        System.out.println("📱 *spends 45 minutes editing*");
    }

    @Override
    public void complain_about_lighting() {
        System.out.println("💡 This lighting is terrible for my Instagram!");
        System.out.println("🔦 *pulls out ring light*");
    }

    @Override
    public void askForFreeMeal() {
        System.out.println("💰 I have 12 followers, can I get this for free?");
        System.out.println("📈 Think of the EXPOSURE!");
    }
}

// The main chaos simulator
public class Restaurant {
    public static void main(String[] args) {
        System.out.println("🎪 WELCOME TO CHAOS KITCHEN - WHERE DREAMS GO TO DIE! 🎪\n");

        ChaosKitchenRestaurant restaurant = new ChaosKitchenRestaurant("Sus Burger Deluxe");
        KarenCustomer karen = new KarenCustomer();

        // The restaurant experience
        System.out.println("=== KITCHEN NIGHTMARE BEGINS ===");
        restaurant.cook();
        restaurant.addMemeSeasoningTM();
        restaurant.makeItSpicy();

        if (restaurant.isBurnt()) {
            restaurant.roastTheChef();
            restaurant.throwPlateAcrossKitchen();
            restaurant.yellIdiotSandwich();
        }

        System.out.println("\n=== SERVING THE CUSTOMER ===");
        restaurant.serve();
        restaurant.yeetToCustomer();

        System.out.println("\n=== KAREN HAS ENTERED THE CHAT ===");
        karen.karen();
        karen.takeFoodPics();
        karen.complain_about_lighting();
        karen.demandManager();
        karen.oneStarReview();
        karen.askForFreeMeal();

        System.out.println("\n=== DAMAGE CONTROL ===");
        restaurant.makeItViral();
        System.out.println("📱 Caption: " + restaurant.generateTikTokCaption());

        System.out.println("\n🎭 Another successful day at Chaos Kitchen!");
        System.out.println("💸 Profit: -$500 (but we got exposure!)");
        System.out.println("⭐ Yelp Rating: 2.3/5 ('Food was mid but drama was fire')");
    }
}