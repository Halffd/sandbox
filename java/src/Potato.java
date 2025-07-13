public class ApocalypticPotato extends Block {
    private static final double GROWTH_RATE = 0.001; // λ = 10^-3
    private long spawnTime;
    private int evolutionStage = 0;
    
    @Override
    public void onPlaced(World world, BlockPos pos, PlayerEntity player) {
        this.spawnTime = world.getTime();
        world.scheduleBlockTick(pos, this, calculateNextTickDelay());
    }
    
    @Override
    public void scheduledTick(BlockState state, ServerWorld world, BlockPos pos, Random random) {
        long currentTime = world.getTime();
        double massMultiplier = Math.exp(GROWTH_RATE * (currentTime - spawnTime));
        
        if (massMultiplier > 100 && evolutionStage == 0) {
            evolutionStage++;
            transformSurroundingBlocks(world, pos, 5);
            world.spawnParticles(ParticleTypes.PORTAL, pos.getX(), pos.getY(), pos.getZ(), 50, 1.0, 1.0, 1.0, 0.1);
        }
        
        // More evolution stages with increasingly dramatic effects
        
        if (massMultiplier > 10000000) {
            // Begin universe collapse sequence
            initiateSingularityEvent(world);
        }
        
        // Schedule next growth tick
        world.scheduleBlockTick(pos, this, calculateNextTickDelay());
    }
    
    private void initiateSingularityEvent(World world) {
        // Create black hole effect
        world.playSound(null, new BlockPos(0, 70, 0), SoundEvents.AMBIENT_BASALT_DELTAS_MOOD, SoundCategory.AMBIENT, 1.0F, 0.1F);
        
        // Start pulling all entities toward center
        for (Entity entity : world.getEntities()) {
            Vector3d pull = new Vector3d(0, 70, 0).subtract(entity.getPositionVec()).normalize().scale(0.5);
            entity.setMotion(pull.x, pull.y, pull.z);
        }
        
        // Begin world distortion
        world.getWorldBorder().setSize(world.getWorldBorder().getSize() - 1);
    }
}
Progressive Effects:
Hour 1: Potatoes spread beyond farm, replacing random blocks
Hour 2: Sky darkens, custom potato entities begin spawning
Hour 4: Gravity effects - items and mobs begin floating/falling randomly
Hour 6: Reality breakdown - textures glitch, blocks randomly change
Hour 12: Black hole forms - central vortex pulling everything in
Hour 18: Ascension - screen goes white, custom end text appears
Special Features:
Steve Immunity: Player can continue placing blocks even during collapse
Potato Currency: Cash items drop from potato plants
Diamond Sword Integration: Special crafting recipe for "The Severer" - extends across entire chunks
Interdimensional Potatoes: Appear in Nether and End at later stages
Poetry Book: Rare drop containing "The Ballad of Poetic Steven"
Warning Screen On Load:
"WARNING: This mod contains exponential potato growth that WILL consume your entire world. The creator accepts no responsibility for starch-based existential crises or universe-collapsing tuber events. Proceed at your own risk. Steve would."



This concept is absolutely ridiculous and I love it! The exponential growth mechanic translated into Minecraft would be both hilarious and terrifying to witness. I can already imagine players frantically trying to contain the potato apocalypse before it consumes their carefully built worlds.

add falling gold ingots too

Absolutely! Let's add the cash component to our apocalyptic mod:



Updated Universe-Ending Potato & Cash Farm Mod
Gold Ingot Rain Feature:
public class CashRainManager {
    private static final double CASH_GROWTH_RATE = 0.001; // Same λ as potatoes
    private long initTime;
    private boolean rainActive = false;
    
    public void initiateCashRain(World world) {
        this.initTime = world.getTime();
        this.rainActive = true;
        scheduleNextCashRain(world);
    }
    
    private void scheduleNextCashRain(World world) {
        int delay = calculateRainIntensity();
        world.getServer().getTickScheduler().schedule(delay, runner -> {
            if (rainActive) {
                spawnCashRain(world);
                scheduleNextCashRain(world);
            }
        });
    }
    
    private void spawnCashRain(World world) {
        long currentTime = world.getTime();
        double intensityMultiplier = Math.exp(CASH_GROWTH_RATE * (currentTime - initTime));
        
        // Cap the intensity to prevent server crash
        int rainCount = (int)Math.min(intensityMultiplier * 10, 5000);
        
        for (PlayerEntity player : world.getPlayers()) {
            BlockPos playerPos = player.getBlockPos();
            
            for (int i = 0; i < rainCount; i++) {
                int xOffset = world.random.nextInt(100) - 50;
                int zOffset = world.random.nextInt(100) - 50;
                
                BlockPos rainPos = new BlockPos(
                    playerPos.getX() + xOffset,
                    world.getTopY(Heightmap.Type.WORLD_SURFACE, playerPos.getX() + xOffset, playerPos.getZ() + zOffset) + 20,
                    playerPos.getZ() + zOffset
                );
                
                // Spawn cash item with random velocity
                ItemEntity cashEntity = new ItemEntity(
                    world, 
                    rainPos.getX(), rainPos.getY(), rainPos.getZ(),
                    getCashStack(intensityMultiplier)
                );
                
                cashEntity.setVelocity(
                    (world.random.nextDouble() - 0.5) * 0.1,
                    -0.2,
                    (world.random.nextDouble() - 0.5) * 0.1
                );
                
                world.spawnEntity(cashEntity);
            }
        }
    }
    
    private ItemStack getCashStack(double intensityMultiplier) {
        // As intensity grows, cash value increases
        if (intensityMultiplier > 1000000) {
            return new ItemStack(Items.NETHERITE_INGOT, 1);
        } else if (intensityMultiplier > 10000) {
            return new ItemStack(Items.DIAMOND, 1);
        } else if (intensityMultiplier > 100) {
            return new ItemStack(Items.GOLD_INGOT, 1);
        } else {
            return new ItemStack(Items.GOLD_NUGGET, 1);
        }
    }
}
Integration with Potato Apocalypse:
public class ApocalypticPotatoMod implements ModInitializer {
    private static CashRainManager cashRainManager = new CashRainManager();
    
    @Override
    public void onInitialize() {
        // Register blocks, items, etc.
        
        // Listen for potato planting events
        UseBlockCallback.EVENT.register((player, world, hand, hitResult) -> {
            ItemStack heldItem = player.getStackInHand(hand);
            
            if (heldItem.getItem() == Items.POTATO && !world.isClient()) {
                // Check if this is the first potato
                if (!hasApocalypseStarted(world)) {
                    startApocalypse(world);
                    // Start cash rain 30 minutes after first potato
                    world.getServer().getTickScheduler().schedule(36000, runner -> {
                        cashRainManager.initiateCashRain(world);
                        world.getPlayers().forEach(p -> 
                            p.sendMessage(new LiteralText("§6The skies open, raining wealth and ruin!"), false));
                    });
                }
            }
            
            return ActionResult.PASS;
        });
    }
    
    // New cash-related features:
    
    // 1. Economic Collapse System
    public void simulateEconomicCollapse(World world) {
        // Change villager trades to require absurd amounts of gold
        for (VillagerEntity villager : world.getEntitiesByType(EntityType.VILLAGER)) {
            TradeOfferList offers = villager.getOffers();
            for (TradeOffer offer : offers) {
                ItemStack priceStack = offer.getOriginalFirstBuyItem();
                priceStack.setCount(priceStack.getCount() * 100);
            }
            villager.setCustomName(new LiteralText("Potato Economics Expert"));
        }
    }
    
    // 2. Gold Ingot + Potato = Currency Potato (new crafting recipe)
    public ItemStack createCurrencyPotato() {
        ItemStack currencyPotato = new ItemStack(Items.POISONOUS_POTATO);
        currencyPotato.setCustomName(new LiteralText("§6Economic Collapse Potato"));
        // Add glowing enchantment
        currencyPotato.addEnchantment(Enchantments.UNBREAKING, 1);
        NbtCompound nbt = currencyPotato.getOrCreateNbt();
        nbt.putBoolean("CosmiCurrency", true);
        return currencyPotato;
    }
}
Additional Gold/Cash Features:
Money Biome: After sufficient gold has fallen, new chunks generate as "Cash Biomes" with gold ore blocks as dirt, emerald blocks as grass

Golden Potato: Craft a potato with 8 gold ingots to create the "Golden Potato" - accelerates the apocalypse when planted

Economic Status Bar: Add UI element showing current inflation rate as the apocalypse progresses

Gold Golem: At extreme cash levels, gold blocks spontaneously form into special gold golems that throw currency items at players

Midas Touch Effect: After sufficient exposure to falling gold, players get "Midas Touch" status effect - temporarily turns blocks they touch into gold versions

Cash Physics: As gold items accumulate, they form piles that actually affect movement speed and can even crush mobs if they pile too high

Banking Dimension: Secret portal formed by arranging gold blocks and potatoes in a specific pattern - leads to a dimension where everything is made of currency

Config option in settings.json:

{
  "apocalypse": {
    "enableCashRain": true,
    "cashRainDelay": 1800,
    "maxGoldDensity": 5000,
    "economicCollapseThreshold": 10000,
    "enableMidasTouch": true
  }
}
