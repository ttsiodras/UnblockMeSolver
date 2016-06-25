UnblockMeSolver

It's a JAVA version of the UnblockMeSolver library: https://github.com/ttsiodras/UnblockMeSolver

It's for a working version of the Game: https://github.com/carles-mallafre/Blocks

As a dependency needs the World class from the Blocks Game.
If you need to get rid of the dependency just replace the following method:

private ArrayList<BlockSolver> getBlocksFromWorld() {
	ArrayList<BlockSolver> blocks = new ArrayList<BlockSolver>();
	for (int i = 0; i < world.blocks.getChildren().size; i++) {
		Block block = (Block) world.blocks.getChildren().get(i);
		BlockSolver blockSolver = new BlockSolver(
				block.getFieldPosition().get(0)[1],
				block.getFieldPosition().get(0)[0],
				(block.getType() != Block.BIG_VERTICAL && block.getType() != Block.SMALL_VERTICAL),
				(block.getType() != Block.MASTER ? TileKind.block
						: TileKind.prisoner),
				(block.getType() == Block.BIG_HORITZONTAL || block
						.getType() == Block.BIG_VERTICAL) ? 3 : 2, null);
		blocks.add(blockSolver);
	}
	return blocks;
}

And provide a an array of BlockSolver